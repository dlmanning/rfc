//! Code generation for the define_library! macro.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::ast::{
    Arity, ConstructAction, ControlPattern, LibraryDef, OpSigDef, PrologKind, StackEffectDef,
};

/// Generate all code for a library definition.
pub fn generate(lib: &LibraryDef) -> TokenStream {
    let struct_def = generate_struct(lib);
    let impl_block = generate_impl(lib);
    let library_impl = generate_library_impl(lib);

    quote! {
        #struct_def
        #impl_block
        #library_impl
    }
}

fn generate_struct(lib: &LibraryDef) -> TokenStream {
    let vis = &lib.vis;
    let name = &lib.name;

    quote! {
        #vis struct #name;
    }
}

fn generate_impl(lib: &LibraryDef) -> TokenStream {
    let name = &lib.name;
    let id = &lib.id;

    // Get control keywords for generating constants and ID match arms
    let control_keywords = collect_control_keywords(lib);
    // Get internal commands for control patterns
    let internal_cmds = collect_control_internal_commands(lib);
    let num_regular_commands = lib.commands.len() as u16;

    // Generate command constants for regular commands
    let cmd_consts: Vec<_> = lib
        .commands
        .iter()
        .enumerate()
        .map(|(i, cmd)| {
            let const_name = format_ident!("CMD_{}", cmd.name.to_string().to_uppercase());
            let i = i as u16;
            quote! {
                const #const_name: u16 = #i;
            }
        })
        .collect();

    // Generate command constants for internal commands (after regular commands)
    let internal_cmd_consts: Vec<_> = internal_cmds
        .iter()
        .enumerate()
        .map(|(i, cmd)| {
            let const_name = format_ident!("CMD_{}", cmd.name);
            let idx = num_regular_commands + i as u16;
            quote! {
                const #const_name: u16 = #idx;
            }
        })
        .collect();

    // Generate command constants for control keywords (after internal commands)
    let num_internal_commands = internal_cmds.len() as u16;
    let control_cmd_consts: Vec<_> = control_keywords
        .iter()
        .enumerate()
        .map(|(i, (kw, _))| {
            let const_name = format_ident!("CMD_{}", kw);
            let idx = num_regular_commands + num_internal_commands + i as u16;
            quote! {
                const #const_name: u16 = #idx;
            }
        })
        .collect();

    // Generate command_id match arms (only for non-internal commands)
    // Include aliases in the same match arm
    let id_arms: Vec<_> = lib
        .commands
        .iter()
        .filter(|cmd| !cmd.internal)
        .map(|cmd| {
            let name_upper = cmd.name.to_string().to_uppercase();
            let const_name = format_ident!("CMD_{}", name_upper);

            // Build list of all names: primary + aliases
            let mut all_names = vec![name_upper.clone()];
            for alias in &cmd.aliases {
                all_names.push(alias.to_uppercase());
            }

            quote! {
                #(#all_names)|* => Some(Self::#const_name),
            }
        })
        .collect();

    // Generate command_id match arms for control keywords
    let control_id_arms: Vec<_> = control_keywords
        .iter()
        .filter(|(_, is_keyword)| *is_keyword)
        .map(|(kw, _)| {
            let const_name = format_ident!("CMD_{}", kw);
            quote! {
                #kw => Some(Self::#const_name),
            }
        })
        .collect();

    // Generate command_name match arms
    let name_arms: Vec<_> = lib
        .commands
        .iter()
        .map(|cmd| {
            let name_upper = cmd.name.to_string().to_uppercase();
            let const_name = format_ident!("CMD_{}", name_upper);
            quote! {
                Self::#const_name => Some(#name_upper),
            }
        })
        .collect();

    // Generate command_name match arms for control keywords
    let control_name_arms: Vec<_> = control_keywords
        .iter()
        .map(|(kw, _)| {
            let const_name = format_ident!("CMD_{}", kw);
            quote! {
                Self::#const_name => Some(#kw),
            }
        })
        .collect();

    // Generate command_name match arms for internal commands
    let internal_name_arms: Vec<_> = internal_cmds
        .iter()
        .filter_map(|cmd| {
            // Map internal command to the keyword it should decompile to
            let decompile_name = internal_cmd_decompile_name(cmd);
            decompile_name.map(|name| {
                let const_name = format_ident!("CMD_{}", cmd.name);
                quote! {
                    Self::#const_name => Some(#name),
                }
            })
        })
        .collect();

    // Generate TokenDoc array
    let token_docs: Vec<_> = lib
        .commands
        .iter()
        .filter(|cmd| !cmd.internal)
        .map(|cmd| {
            let name = cmd.name.to_string().to_uppercase();
            let brief = &cmd.doc.brief;
            let example = cmd.doc.example.as_deref().unwrap_or("");
            let see_also: Vec<_> = cmd.doc.see_also.iter().map(|s| quote!(#s)).collect();

            // Generate stack notation: use provided or generate from effect
            let stack_str = if let Some(s) = &cmd.doc.stack {
                s.clone()
            } else {
                match &cmd.effect {
                    StackEffectDef::Fixed { consumes, produces } => {
                        let inputs: Vec<_> = (0..*consumes)
                            .map(|i| ((b'a' + i) as char).to_string())
                            .collect();
                        let outputs: Vec<_> = (0..*produces)
                            .map(|i| ((b'a' + i) as char).to_string())
                            .collect();
                        format!("( {} -- {} )", inputs.join(" "), outputs.join(" "))
                    }
                    StackEffectDef::Dynamic => "( ... -- ... )".to_string(),
                }
            };

            quote! {
                rpl_lang::library::TokenDoc {
                    name: #name,
                    brief: #brief,
                    stack: #stack_str,
                    example: #example,
                    see_also: &[#(#see_also),*],
                }
            }
        })
        .collect();

    // Generate operator_info and precedence helpers if this is an operator syntax library
    let operator_info_fn = generate_operator_info(lib);
    let precedence_fn = generate_precedence(lib);

    // Generate is_falsy helper if needed
    let is_falsy_helper = generate_is_falsy_helper(lib);

    quote! {
        impl #name {
            /// Library ID.
            pub const ID: rpl_lang::library::LibraryId = rpl_lang::library::LibraryId::new(#id);

            #(#cmd_consts)*
            #(#internal_cmd_consts)*
            #(#control_cmd_consts)*

            #is_falsy_helper

            /// Get command ID from name (case-insensitive).
            fn command_id(name: &str) -> Option<u16> {
                match name.to_ascii_uppercase().as_str() {
                    #(#id_arms)*
                    #(#control_id_arms)*
                    _ => None,
                }
            }

            /// Get command name from ID.
            fn command_name(id: u16) -> Option<&'static str> {
                match id {
                    #(#name_arms)*
                    #(#internal_name_arms)*
                    #(#control_name_arms)*
                    _ => None,
                }
            }

            #operator_info_fn
            #precedence_fn

            /// Token documentation.
            const TOKENS: &'static [rpl_lang::library::TokenDoc] = &[
                #(#token_docs),*
            ];
        }
    }
}

fn generate_library_impl(lib: &LibraryDef) -> TokenStream {
    let name = &lib.name;
    let display_name = &lib.display_name;

    let probe_impl = generate_probe(lib);
    let compile_impl = generate_compile(lib);
    let execute_impl = generate_execute(lib);
    let decompile_impl = generate_decompile(lib);
    let stack_effect_impl = generate_stack_effect(lib);
    let register_operators_impl = generate_register_operators(lib);
    let register_coercions_impl = generate_register_coercions(lib);

    quote! {
        impl rpl_lang::library::Library for #name {
            fn id(&self) -> rpl_lang::library::LibraryId {
                Self::ID
            }

            fn name(&self) -> &'static str {
                #display_name
            }

            #probe_impl
            #compile_impl
            #execute_impl
            #decompile_impl
            #stack_effect_impl
            #register_operators_impl
            #register_coercions_impl

            fn tokens(&self) -> &'static [rpl_lang::library::TokenDoc] {
                Self::TOKENS
            }
        }
    }
}

/// Generate operator_info helper function for operator syntax libraries.
fn generate_operator_info(lib: &LibraryDef) -> TokenStream {
    if lib.operator_syntax.is_empty() {
        return quote! {};
    }

    // Separate exact-match tokens (symbols) from case-insensitive tokens (words)
    let mut exact_arms = Vec::new();
    let mut word_arms = Vec::new();

    for op in &lib.operator_syntax {
        let kind_ident = format_ident!("{}", op.kind);
        let arity: u8 = match op.arity {
            Arity::Unary => 1,
            Arity::Binary => 2,
        };
        let is_infix = op.infix_precedence.is_some();

        // Separate exact tokens (symbols) from word tokens (need uppercase)
        let (symbols, words): (Vec<_>, Vec<_>) =
            op.tokens.iter().partition(|t| !t.chars().all(|c| c.is_alphabetic()));

        if !symbols.is_empty() {
            exact_arms.push(quote! {
                #(#symbols)|* => Some((rpl_lang::operator::OperatorKind::#kind_ident, #arity, #is_infix)),
            });
        }

        if !words.is_empty() {
            let words_upper: Vec<_> = words.iter().map(|w| w.to_uppercase()).collect();
            word_arms.push(quote! {
                #(#words_upper)|* => Some((rpl_lang::operator::OperatorKind::#kind_ident, #arity, #is_infix)),
            });
        }
    }

    let word_match = if word_arms.is_empty() {
        quote! { None }
    } else {
        quote! {
            let upper = text.to_ascii_uppercase();
            match upper.as_str() {
                #(#word_arms)*
                _ => None,
            }
        }
    };

    quote! {
        /// Get operator info from text.
        /// Returns (OperatorKind, arity, is_binary_infix) if recognized.
        fn operator_info(text: &str) -> Option<(rpl_lang::operator::OperatorKind, u8, bool)> {
            match text {
                #(#exact_arms)*
                _ => { #word_match }
            }
        }
    }
}

/// Generate precedence helper function for operator syntax libraries.
fn generate_precedence(lib: &LibraryDef) -> TokenStream {
    if lib.operator_syntax.is_empty() {
        return quote! {};
    }

    // Collect all tokens with their precedence values
    let mut arms = Vec::new();

    for op in &lib.operator_syntax {
        if let Some(prec) = op.infix_precedence {
            // Include all tokens (both symbols and words)
            let all_tokens: Vec<_> = op
                .tokens
                .iter()
                .flat_map(|t| {
                    if t.chars().all(|c| c.is_alphabetic()) {
                        // Word - include both original and uppercase
                        vec![t.clone(), t.to_uppercase()]
                    } else {
                        vec![t.clone()]
                    }
                })
                .collect();

            arms.push(quote! {
                #(#all_tokens)|* => #prec,
            });
        }
    }

    quote! {
        /// Get the precedence for an operator (higher = binds tighter).
        fn precedence(text: &str) -> u8 {
            match text {
                #(#arms)*
                _ => 0,
            }
        }
    }
}

fn generate_probe(lib: &LibraryDef) -> TokenStream {
    if let Some(custom) = &lib.custom_probe {
        quote! {
            fn probe(&self, ctx: &rpl_lang::library::ProbeContext) -> rpl_lang::library::ProbeResult {
                #custom
            }
        }
    } else if !lib.operator_syntax.is_empty() {
        // Operator syntax library - special probe handling
        // Generate match arms for right-associative operators
        let right_assoc_tokens: Vec<_> = lib
            .operator_syntax
            .iter()
            .filter(|op| op.right_assoc)
            .flat_map(|op| op.tokens.clone())
            .collect();

        let right_assoc_check = if right_assoc_tokens.is_empty() {
            quote! { false }
        } else {
            quote! {
                match text {
                    #(#right_assoc_tokens)|* => true,
                    _ => false,
                }
            }
        };

        quote! {
            fn probe(&self, ctx: &rpl_lang::library::ProbeContext) -> rpl_lang::library::ProbeResult {
                let text = ctx.text();

                if let Some((_, arity, is_binary_infix)) = Self::operator_info(text) {
                    let info = if is_binary_infix && ctx.in_infix() {
                        // In infix mode, return binary operator info with precedence
                        let prec = Self::precedence(text);
                        let is_right = #right_assoc_check;
                        if is_right {
                            rpl_core::token::TokenInfo::binary_right(text.len() as u8, prec)
                        } else {
                            rpl_core::token::TokenInfo::binary_left(text.len() as u8, prec)
                        }
                    } else {
                        // In RPN mode, operators are just atoms
                        rpl_core::token::TokenInfo::atom(text.len() as u8)
                    };

                    rpl_lang::library::ProbeResult::Match {
                        info,
                        semantic: rpl_core::token::SemanticKind::Operator,
                    }
                } else {
                    rpl_lang::library::ProbeResult::NoMatch
                }
            }
        }
    } else {
        // Generate literal probes
        let literal_probes: Vec<_> = lib
            .literals
            .iter()
            .map(|lit| {
                let codec = &lit.codec;
                quote! {
                    if let Some(len) = <#codec as rpl_lang::library::LiteralCodec>::probe(text) {
                        return rpl_lang::library::ProbeResult::Match {
                            info: rpl_core::token::TokenInfo::atom(len as u8),
                            semantic: <#codec as rpl_lang::library::LiteralCodec>::semantic(),
                        };
                    }
                }
            })
            .collect();

        // Generate construct match arms
        let construct_arms: Vec<_> = lib
            .constructs
            .iter()
            .map(|c| {
                let token = &c.token;
                let (bracket_fn, len) = match &c.action {
                    ConstructAction::Open(_) => ("open_bracket", token.len() as u8),
                    ConstructAction::Close => ("close_bracket", token.len() as u8),
                };
                let bracket_fn = format_ident!("{}", bracket_fn);
                quote! {
                    #token => rpl_lang::library::ProbeResult::Match {
                        info: rpl_core::token::TokenInfo::#bracket_fn(#len),
                        semantic: rpl_core::token::SemanticKind::Bracket,
                    },
                }
            })
            .collect();

        // Generate control keyword probe arms
        let control_probe_arms = generate_control_probe_arms(lib);

        let has_literals = !literal_probes.is_empty();
        let has_constructs = !construct_arms.is_empty();
        let has_control = !control_probe_arms.is_empty();

        // Generate the probe function with all applicable match arms
        // Control keywords should be checked before regular commands
        if has_literals || has_constructs || has_control {
            let control_match = if has_control {
                quote! {
                    // Check control keywords (case-insensitive)
                    match text.to_ascii_uppercase().as_str() {
                        #(#control_probe_arms)*
                        _ => {}
                    }
                }
            } else {
                quote! {}
            };

            if has_constructs {
                quote! {
                    fn probe(&self, ctx: &rpl_lang::library::ProbeContext) -> rpl_lang::library::ProbeResult {
                        let text = ctx.text();
                        // Check literals first
                        #(#literal_probes)*
                        // Then check constructs
                        match text {
                            #(#construct_arms)*
                            _ => {
                                #control_match
                                // Then check commands
                                if Self::command_id(text).is_some() {
                                    rpl_lang::library::ProbeResult::Match {
                                        info: rpl_core::token::TokenInfo::atom(text.len() as u8),
                                        semantic: rpl_core::token::SemanticKind::Command,
                                    }
                                } else {
                                    rpl_lang::library::ProbeResult::NoMatch
                                }
                            }
                        }
                    }
                }
            } else {
                quote! {
                    fn probe(&self, ctx: &rpl_lang::library::ProbeContext) -> rpl_lang::library::ProbeResult {
                        let text = ctx.text();
                        // Check literals first
                        #(#literal_probes)*
                        #control_match
                        // Then check commands
                        if Self::command_id(text).is_some() {
                            rpl_lang::library::ProbeResult::Match {
                                info: rpl_core::token::TokenInfo::atom(text.len() as u8),
                                semantic: rpl_core::token::SemanticKind::Command,
                            }
                        } else {
                            rpl_lang::library::ProbeResult::NoMatch
                        }
                    }
                }
            }
        } else {
            quote! {
                fn probe(&self, ctx: &rpl_lang::library::ProbeContext) -> rpl_lang::library::ProbeResult {
                    let text = ctx.text();
                    if Self::command_id(text).is_some() {
                        rpl_lang::library::ProbeResult::Match {
                            info: rpl_core::token::TokenInfo::atom(text.len() as u8),
                            semantic: rpl_core::token::SemanticKind::Command,
                        }
                    } else {
                        rpl_lang::library::ProbeResult::NoMatch
                    }
                }
            }
        }
    }
}

fn generate_compile(lib: &LibraryDef) -> TokenStream {
    if let Some(custom) = &lib.custom_compile {
        quote! {
            fn compile(&self, ctx: &mut rpl_lang::library::CompileContext) -> rpl_lang::library::CompileResult {
                #custom
            }
        }
    } else if !lib.operator_syntax.is_empty() {
        // Operator syntax library - compiler handles operators specially via stack_effect
        quote! {
            fn compile(&self, _ctx: &mut rpl_lang::library::CompileContext) -> rpl_lang::library::CompileResult {
                // The compiler handles Operator stack effects via the registry.
                // This library just recognizes the syntax; actual bytecode emission
                // is done by the compiler based on stack_effect().
                rpl_lang::library::CompileResult::Ok
            }
        }
    } else {
        // Generate literal compile blocks
        let literal_compiles: Vec<_> = lib
            .literals
            .iter()
            .map(|lit| {
                let codec = &lit.codec;
                quote! {
                    if let Ok(value) = <#codec as rpl_lang::library::LiteralCodec>::parse(text) {
                        let data = <#codec as rpl_lang::library::LiteralCodec>::encode(&value);
                        let type_id = <#codec as rpl_lang::library::LiteralCodec>::type_id();
                        ctx.emit_prolog(type_id.as_u16(), data.len() as u16);
                        for word in data {
                            ctx.emit(word);
                        }
                        return rpl_lang::library::CompileResult::Ok;
                    }
                }
            })
            .collect();

        // Generate construct match arms
        let construct_arms: Vec<_> = lib
            .constructs
            .iter()
            .map(|c| {
                let token = &c.token;
                match &c.action {
                    ConstructAction::Open(kind) => {
                        let kind_ident = format_ident!("{}", kind);
                        quote! {
                            #token => rpl_lang::library::CompileResult::StartConstruct {
                                kind: rpl_lang::library::ConstructKind::#kind_ident,
                            },
                        }
                    }
                    ConstructAction::Close => {
                        quote! {
                            #token => rpl_lang::library::CompileResult::EndConstruct,
                        }
                    }
                }
            })
            .collect();

        // Generate control keyword compile arms
        let control_compile_arms = generate_control_compile_arms(lib);

        let has_literals = !literal_compiles.is_empty();
        let has_constructs = !construct_arms.is_empty();
        let has_control = !control_compile_arms.is_empty();

        // Generate compile function with all applicable match arms
        // Control keywords need special handling (StartConstruct, NeedMore, etc.)
        let control_match = if has_control {
            quote! {
                // Check control keywords (case-insensitive)
                match text.to_ascii_uppercase().as_str() {
                    #(#control_compile_arms)*
                    _ => {}
                }
            }
        } else {
            quote! {}
        };

        if has_constructs {
            quote! {
                fn compile(&self, ctx: &mut rpl_lang::library::CompileContext) -> rpl_lang::library::CompileResult {
                    let text = ctx.text();
                    // Try to compile as literal first
                    #(#literal_compiles)*
                    // Then check constructs
                    match text {
                        #(#construct_arms)*
                        _ => {
                            #control_match
                            // Then check commands
                            if let Some(cmd) = Self::command_id(text) {
                                ctx.emit_opcode(Self::ID.as_u16(), cmd);
                                rpl_lang::library::CompileResult::Ok
                            } else {
                                rpl_lang::library::CompileResult::NoMatch
                            }
                        }
                    }
                }
            }
        } else if has_literals || has_control {
            quote! {
                fn compile(&self, ctx: &mut rpl_lang::library::CompileContext) -> rpl_lang::library::CompileResult {
                    let text = ctx.text();
                    // Try to compile as literal first
                    #(#literal_compiles)*
                    #control_match
                    // Then check commands
                    if let Some(cmd) = Self::command_id(text) {
                        ctx.emit_opcode(Self::ID.as_u16(), cmd);
                        rpl_lang::library::CompileResult::Ok
                    } else {
                        rpl_lang::library::CompileResult::NoMatch
                    }
                }
            }
        } else {
            quote! {
                fn compile(&self, ctx: &mut rpl_lang::library::CompileContext) -> rpl_lang::library::CompileResult {
                    let text = ctx.text();
                    if let Some(cmd) = Self::command_id(text) {
                        ctx.emit_opcode(Self::ID.as_u16(), cmd);
                        rpl_lang::library::CompileResult::Ok
                    } else {
                        rpl_lang::library::CompileResult::NoMatch
                    }
                }
            }
        }
    }
}

fn generate_execute(lib: &LibraryDef) -> TokenStream {
    let name = &lib.name;
    let lib_name_str = name.to_string();

    // Operator syntax libraries don't execute anything
    if !lib.operator_syntax.is_empty() && lib.commands.is_empty() && lib.control_patterns.is_empty()
    {
        return quote! {
            fn execute(&self, _ctx: &mut rpl_lang::library::ExecuteContext) -> rpl_lang::library::ExecuteResult {
                // Operator syntax libraries don't execute anything.
                // Operators are dispatched to type-owning libraries.
                Err(format!("{} has no executable commands", #lib_name_str))
            }
        };
    }

    // Generate arms for regular commands
    let arms: Vec<_> = lib
        .commands
        .iter()
        .map(|cmd| {
            let const_name = format_ident!("CMD_{}", cmd.name.to_string().to_uppercase());
            let body = &cmd.body;
            quote! {
                Self::#const_name => {
                    #body
                }
            }
        })
        .collect();

    // Generate arms for internal commands from control patterns
    let internal_cmds = collect_control_internal_commands(lib);
    let internal_arms: Vec<_> = internal_cmds
        .iter()
        .map(|cmd| {
            let const_name = format_ident!("CMD_{}", cmd.name);
            let body = generate_internal_cmd_execute(cmd);
            quote! {
                Self::#const_name => {
                    #body
                }
            }
        })
        .collect();

    // Generate execute arms for InlineConditional keywords (IFT, IFTE)
    // These are direct commands handled by keywords, not internal_cmds
    let inline_cond_arms: Vec<_> = lib
        .control_patterns
        .iter()
        .filter(|p| p.pattern == ControlPattern::InlineConditional)
        .flat_map(|pattern| {
            let ift = pattern.keywords[0].to_string().to_uppercase();
            let ifte = pattern.keywords[1].to_string().to_uppercase();
            let ift_const = format_ident!("CMD_{}", ift);
            let ifte_const = format_ident!("CMD_{}", ifte);

            vec![
                quote! {
                    Self::#ift_const => {
                        // IFT: condition obj → obj (if true) or nothing (if false)
                        let obj = match ctx.pop() {
                            Ok(v) => v,
                            Err(_) => return Err("Stack underflow".to_string()),
                        };
                        let condition = match ctx.pop() {
                            Ok(v) => v,
                            Err(_) => return Err("Stack underflow".to_string()),
                        };
                        if !Self::is_falsy(&condition) {
                            ctx.push(obj).map_err(|_| "Stack overflow")?;
                        }
                        Ok(rpl_lang::library::ExecuteOk::Ok)
                    }
                },
                quote! {
                    Self::#ifte_const => {
                        // IFTE: condition true_obj false_obj → selected_obj
                        let false_obj = match ctx.pop() {
                            Ok(v) => v,
                            Err(_) => return Err("Stack underflow".to_string()),
                        };
                        let true_obj = match ctx.pop() {
                            Ok(v) => v,
                            Err(_) => return Err("Stack underflow".to_string()),
                        };
                        let condition = match ctx.pop() {
                            Ok(v) => v,
                            Err(_) => return Err("Stack underflow".to_string()),
                        };
                        if Self::is_falsy(&condition) {
                            ctx.push(false_obj).map_err(|_| "Stack overflow")?;
                        } else {
                            ctx.push(true_obj).map_err(|_| "Stack overflow")?;
                        }
                        Ok(rpl_lang::library::ExecuteOk::Ok)
                    }
                },
            ]
        })
        .collect();

    quote! {
        fn execute(&self, ctx: &mut rpl_lang::library::ExecuteContext) -> rpl_lang::library::ExecuteResult {
            match ctx.cmd() {
                #(#arms)*
                #(#internal_arms)*
                #(#inline_cond_arms)*
                _ => Err(format!("Unknown {} command: {}", #lib_name_str, ctx.cmd())),
            }
        }
    }
}

fn generate_decompile(lib: &LibraryDef) -> TokenStream {
    // If custom_decompile is provided, use it for the entire decompile implementation
    if let Some(custom) = &lib.custom_decompile {
        return quote! {
            fn decompile(&self, ctx: &mut rpl_lang::library::DecompileContext) -> rpl_lang::library::DecompileResult {
                #custom
            }
        };
    }

    // Operator syntax libraries don't emit bytecode, so nothing to decompile
    if !lib.operator_syntax.is_empty() && lib.commands.is_empty() && lib.prologs.is_empty() {
        return quote! {
            fn decompile(&self, _ctx: &mut rpl_lang::library::DecompileContext) -> rpl_lang::library::DecompileResult {
                // Operator syntax libraries don't emit bytecode, so nothing to decompile.
                // Operator bytecode comes from type-owning libraries or dispatch.
                rpl_lang::library::DecompileResult::Unknown
            }
        };
    }

    // Generate prolog handler
    // Check prologs section, then literals, then custom if provided, otherwise return Unknown

    // Generate prolog checks (using if-else chain since type_id.as_u16() is not a pattern)
    // IMPORTANT: Each check must consume the prolog ONLY if it handles the type.
    // If a check doesn't match, it should NOT consume the prolog so other libraries can try.
    let prolog_checks: Vec<_> = lib
        .prologs
        .iter()
        .map(|prolog| {
            let type_id = &prolog.type_id;
            match &prolog.kind {
                PrologKind::Delimited { open, close } => {
                    quote! {
                        if type_id == rpl_core::TypeId::#type_id.as_u16() {
                            ctx.read(); // consume prolog now that we're handling it
                            ctx.write(#open);
                            if size > 0 {
                                ctx.write(" ");
                                ctx.decompile_inner(size);
                            }
                            ctx.write(" ");
                            ctx.write(#close);
                            return rpl_lang::library::DecompileResult::Ok;
                        }
                    }
                }
                PrologKind::Format(codec) => {
                    quote! {
                        if type_id == rpl_core::TypeId::#type_id.as_u16() {
                            ctx.read(); // consume prolog now that we're handling it
                            // Read data words
                            let mut words = Vec::with_capacity(size);
                            for _ in 0..size {
                                if let Some(w) = ctx.read() {
                                    words.push(w);
                                }
                            }
                            // Decode and format
                            if let Some(value) = <#codec as rpl_lang::library::LiteralCodec>::decode(&words) {
                                ctx.write(&<#codec as rpl_lang::library::LiteralCodec>::format(&value));
                                return rpl_lang::library::DecompileResult::Ok;
                            }
                        }
                    }
                }
                PrologKind::Custom(body) => {
                    quote! {
                        if type_id == rpl_core::TypeId::#type_id.as_u16() {
                            ctx.read(); // consume prolog now that we're handling it
                            #body
                        }
                    }
                }
            }
        })
        .collect();

    // Generate literal decompile blocks (for backwards compatibility with literals section)
    // IMPORTANT: Only consume the prolog if the type matches.
    let literal_decompiles: Vec<_> = lib
        .literals
        .iter()
        .map(|lit| {
            let codec = &lit.codec;
            quote! {
                if type_id == <#codec as rpl_lang::library::LiteralCodec>::type_id().as_u16() {
                    ctx.read(); // consume prolog now that we're handling it
                    // Read data words
                    let mut words = Vec::with_capacity(size);
                    for _ in 0..size {
                        if let Some(w) = ctx.read() {
                            words.push(w);
                        }
                    }
                    // Decode and format
                    if let Some(value) = <#codec as rpl_lang::library::LiteralCodec>::decode(&words) {
                        ctx.write(&<#codec as rpl_lang::library::LiteralCodec>::format(&value));
                        return rpl_lang::library::DecompileResult::Ok;
                    }
                }
            }
        })
        .collect();

    let has_prologs = !prolog_checks.is_empty();
    let has_literals = !literal_decompiles.is_empty();
    let has_custom = lib.custom_decompile_prolog.is_some();

    let prolog_handler = if has_prologs || has_literals || has_custom {
        let custom_handler = if let Some(custom) = &lib.custom_decompile_prolog {
            quote! { #custom }
        } else {
            quote! { rpl_lang::library::DecompileResult::Unknown }
        };

        let prolog_if_chain = if has_prologs {
            quote! { #(#prolog_checks)* }
        } else {
            quote! {}
        };

        let literal_if_chain = if has_literals {
            quote! { #(#literal_decompiles)* }
        } else {
            quote! {}
        };

        quote! {
            if let Some(word) = ctx.peek()
                && rpl_core::is_prolog(word)
            {
                let type_id = rpl_core::extract_type(word);
                let size = rpl_core::extract_size(word) as usize;
                // NOTE: Don't consume prolog here - each check consumes it if it matches.
                // This allows other libraries to try if we don't handle this type.

                // Check declarative prologs
                #prolog_if_chain

                // Check literal codecs
                #literal_if_chain

                // Fall through to custom handler or unknown
                #custom_handler
            } else {
                rpl_lang::library::DecompileResult::Unknown
            }
        }
    } else {
        quote! { rpl_lang::library::DecompileResult::Unknown }
    };

    // Generate decompile arms for internal commands with operands
    let control_decompile_arms = generate_control_decompile_arms(lib);
    let has_control_decompile = !control_decompile_arms.is_empty();

    let call_handler = if has_control_decompile {
        quote! {
            // Handle internal commands with operands first
            match cmd {
                #(#control_decompile_arms)*
                _ => {}
            }
            // Then try command_name for regular commands and markers
            if let Some(name) = Self::command_name(cmd) {
                ctx.write(name);
                rpl_lang::library::DecompileResult::Ok
            } else {
                rpl_lang::library::DecompileResult::Unknown
            }
        }
    } else {
        quote! {
            if let Some(name) = Self::command_name(cmd) {
                ctx.write(name);
                rpl_lang::library::DecompileResult::Ok
            } else {
                rpl_lang::library::DecompileResult::Unknown
            }
        }
    };

    quote! {
        fn decompile(&self, ctx: &mut rpl_lang::library::DecompileContext) -> rpl_lang::library::DecompileResult {
            match ctx.mode() {
                rpl_lang::library::DecompileMode::Prolog => { #prolog_handler }
                rpl_lang::library::DecompileMode::Call(cmd) => {
                    #call_handler
                }
            }
        }
    }
}

fn generate_stack_effect(lib: &LibraryDef) -> TokenStream {
    // If custom_stack_effect is provided, use it for the entire stack_effect implementation
    if let Some(custom) = &lib.custom_stack_effect {
        return quote! {
            fn stack_effect(&self, token: &str) -> rpl_lang::library::StackEffect {
                #custom
            }
        };
    }

    // Operator syntax libraries return StackEffect::Operator for operators
    if !lib.operator_syntax.is_empty() {
        return quote! {
            fn stack_effect(&self, token: &str) -> rpl_lang::library::StackEffect {
                if let Some((kind, arity, _)) = Self::operator_info(token) {
                    rpl_lang::library::StackEffect::Operator { kind, arity }
                } else {
                    rpl_lang::library::StackEffect::Dynamic
                }
            }
        };
    }

    // Generate literal checks (literals produce 1 value, consume 0)
    let literal_checks: Vec<_> = lib
        .literals
        .iter()
        .map(|lit| {
            let codec = &lit.codec;
            quote! {
                if <#codec as rpl_lang::library::LiteralCodec>::probe(token).is_some() {
                    return rpl_lang::library::StackEffect::Fixed {
                        consumes: 0,
                        produces: 1,
                    };
                }
            }
        })
        .collect();

    // Generate construct match arms (these match on exact token, not uppercased)
    let construct_arms: Vec<_> = lib
        .constructs
        .iter()
        .map(|c| {
            let token = &c.token;
            match &c.action {
                ConstructAction::Open(_) => {
                    quote! {
                        #token => return rpl_lang::library::StackEffect::StartConstruct,
                    }
                }
                ConstructAction::Close => {
                    quote! {
                        #token => return rpl_lang::library::StackEffect::EndConstruct,
                    }
                }
            }
        })
        .collect();

    let command_arms: Vec<_> = lib
        .commands
        .iter()
        .filter(|cmd| !cmd.internal)
        .filter_map(|cmd| {
            let name_upper = cmd.name.to_string().to_uppercase();
            match &cmd.effect {
                StackEffectDef::Fixed { consumes, produces } => {
                    // Build list of all names: primary + aliases
                    let mut all_names = vec![name_upper.clone()];
                    for alias in &cmd.aliases {
                        all_names.push(alias.to_uppercase());
                    }

                    Some(quote! {
                        #(#all_names)|* => rpl_lang::library::StackEffect::Fixed {
                            consumes: #consumes,
                            produces: #produces,
                        },
                    })
                }
                StackEffectDef::Dynamic => None, // Will fall through to default
            }
        })
        .collect();

    // Generate control keyword stack effect arms
    let control_stack_effect_arms = generate_control_stack_effect_arms(lib);

    let has_constructs = !construct_arms.is_empty();

    // Generate stack_effect function with all applicable match arms
    let construct_match = if has_constructs {
        quote! {
            // Check constructs first (exact match)
            match token {
                #(#construct_arms)*
                _ => {}
            }
        }
    } else {
        quote! {}
    };

    quote! {
        fn stack_effect(&self, token: &str) -> rpl_lang::library::StackEffect {
            // Check literals first
            #(#literal_checks)*
            #construct_match
            // Then check commands and control keywords (case-insensitive)
            match token.to_ascii_uppercase().as_str() {
                #(#control_stack_effect_arms)*
                #(#command_arms)*
                _ => rpl_lang::library::StackEffect::Dynamic,
            }
        }
    }
}

/// Collect all keywords from control patterns.
/// Returns a list of (keyword_name, is_keyword) pairs.
/// Keywords are user-visible (IF, THEN, etc.), non-keywords are internal markers.
fn collect_control_keywords(lib: &LibraryDef) -> Vec<(String, bool)> {
    let mut keywords = Vec::new();

    for pattern in &lib.control_patterns {
        // Add all positional keywords
        for kw in &pattern.keywords {
            keywords.push((kw.to_string().to_uppercase(), true));
        }

        // Add optional keywords from options
        if let Some(alt) = &pattern.options.alt {
            keywords.push((alt.to_string().to_uppercase(), true));
        }
        if let Some(step) = &pattern.options.step {
            keywords.push((step.to_string().to_uppercase(), true));
        }
        if let Some(no_error) = &pattern.options.no_error {
            keywords.push((no_error.to_string().to_uppercase(), true));
        }
    }

    // Deduplicate (e.g., NEXT appears in multiple patterns)
    keywords.sort_by(|a, b| a.0.cmp(&b.0));
    keywords.dedup_by(|a, b| a.0 == b.0);
    keywords
}

fn generate_register_operators(lib: &LibraryDef) -> TokenStream {
    if lib.operators.is_empty() {
        // No operators - don't override the default implementation
        return quote! {};
    }

    let registrations: Vec<_> = lib
        .operators
        .iter()
        .map(|op| {
            let kind_ident = format_ident!("{}", op.kind);
            let cmd_ident = format_ident!("{}", op.command);
            let priority = op.priority;
            let commutative = op.commutative;

            let (signature_expr, default_type) = match &op.signature {
                OpSigDef::Symmetric(ty) => {
                    let ty_ident = format_ident!("{}", ty);
                    (
                        quote! { rpl_lang::operator::OpSignature::Symmetric(rpl_core::TypeId::#ty_ident) },
                        ty.clone(),
                    )
                }
                OpSigDef::Unary(ty) => {
                    let ty_ident = format_ident!("{}", ty);
                    (
                        quote! { rpl_lang::operator::OpSignature::Unary(rpl_core::TypeId::#ty_ident) },
                        ty.clone(),
                    )
                }
                OpSigDef::Binary { left, right } => {
                    let left_ident = format_ident!("{}", left);
                    let right_ident = format_ident!("{}", right);
                    (
                        quote! {
                            rpl_lang::operator::OpSignature::Binary {
                                left: rpl_core::TypeId::#left_ident,
                                right: rpl_core::TypeId::#right_ident,
                            }
                        },
                        left.clone(), // Default result is left type for binary
                    )
                }
            };

            let result_type = op.result_type.as_ref().unwrap_or(&default_type);
            let result_ident = format_ident!("{}", result_type);

            quote! {
                registry.register_operator(rpl_lang::operator::OperatorRegistration {
                    op: rpl_lang::operator::OperatorKind::#kind_ident,
                    signature: #signature_expr,
                    result: rpl_core::TypeId::#result_ident,
                    lib: Self::ID,
                    command: Self::#cmd_ident,
                    priority: #priority,
                    commutative: #commutative,
                });
            }
        })
        .collect();

    quote! {
        fn register_operators(&self, registry: &mut rpl_lang::operator::OperatorRegistry) {
            #(#registrations)*
        }
    }
}

fn generate_register_coercions(lib: &LibraryDef) -> TokenStream {
    if lib.coercions.is_empty() {
        // No coercions - don't override the default implementation
        return quote! {};
    }

    let registrations: Vec<_> = lib
        .coercions
        .iter()
        .map(|coercion| {
            let from_ident = &coercion.from;
            let to_ident = &coercion.to;
            let cmd_ident = &coercion.command;
            let priority = coercion.priority;
            let implicit = coercion.implicit;

            quote! {
                registry.register_coercion(rpl_lang::operator::CoercionRegistration {
                    from: rpl_core::TypeId::#from_ident,
                    to: rpl_core::TypeId::#to_ident,
                    lib: Self::ID,
                    command: Self::#cmd_ident,
                    implicit: #implicit,
                    priority: #priority,
                });
            }
        })
        .collect();

    quote! {
        fn register_coercions(&self, registry: &mut rpl_lang::operator::OperatorRegistry) {
            #(#registrations)*
        }
    }
}

// =============================================================================
// Control Pattern Code Generation
// =============================================================================

/// Internal command kinds for control flow patterns.
/// Each kind has associated execute semantics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum InternalCmd {
    /// No-op marker for decompilation (e.g., IF_MARKER, END_MARKER)
    Marker,
    /// Conditional jump: pop condition, jump if false
    JumpIfFalse,
    /// Unconditional jump
    Jump,
    /// Loop back if condition is false (for UNTIL)
    UntilJump,
    /// Initialize loop counter from stack (for START)
    LoopSetup,
    /// Increment loop counter and jump if not done (for NEXT)
    LoopNext,
    /// Custom increment from stack and jump (for STEP)
    LoopStep,
    /// Initialize FOR loop with symbol
    ForSetup,
    /// Increment FOR variable and jump if not done
    ForNext,
    /// Custom increment for FOR and jump
    ForStep,
    /// Setup error handler
    IfErrSetup,
    /// Pop error handler on success
    IfErrSuccess,
    /// Inline conditional: IFT (condition obj → obj or nothing)
    Ift,
    /// Inline conditional: IFTE (condition true_obj false_obj → selected)
    Ifte,
}

/// Collected internal command with its name and kind.
#[derive(Debug, Clone)]
struct InternalCmdDef {
    name: String,
    kind: InternalCmd,
}

/// Collect all internal commands needed by control patterns.
/// Returns deduplicated list of (name, kind) pairs.
fn collect_control_internal_commands(lib: &LibraryDef) -> Vec<InternalCmdDef> {
    let mut commands = Vec::new();

    for pattern in &lib.control_patterns {
        match pattern.pattern {
            ControlPattern::IfThenElse => {
                // IF → IF_MARKER, THEN → JUMP_IF_FALSE, ELSE → JUMP, END → END_MARKER
                let open = pattern.keywords[0].to_string().to_uppercase();
                let close = pattern.keywords[2].to_string().to_uppercase();
                commands.push(InternalCmdDef {
                    name: format!("{}_MARKER", open),
                    kind: InternalCmd::Marker,
                });
                commands.push(InternalCmdDef {
                    name: "JUMP_IF_FALSE".to_string(),
                    kind: InternalCmd::JumpIfFalse,
                });
                commands.push(InternalCmdDef {
                    name: "JUMP".to_string(),
                    kind: InternalCmd::Jump,
                });
                commands.push(InternalCmdDef {
                    name: format!("{}_MARKER", close),
                    kind: InternalCmd::Marker,
                });
            }
            ControlPattern::DoUntil => {
                let open = pattern.keywords[0].to_string().to_uppercase();
                commands.push(InternalCmdDef {
                    name: format!("{}_MARKER", open),
                    kind: InternalCmd::Marker,
                });
                commands.push(InternalCmdDef {
                    name: "UNTIL_JUMP".to_string(),
                    kind: InternalCmd::UntilJump,
                });
            }
            ControlPattern::WhileRepeat => {
                let open = pattern.keywords[0].to_string().to_uppercase();
                let test = pattern.keywords[1].to_string().to_uppercase();
                commands.push(InternalCmdDef {
                    name: format!("{}_MARKER", open),
                    kind: InternalCmd::Marker,
                });
                commands.push(InternalCmdDef {
                    name: "JUMP_IF_FALSE".to_string(),
                    kind: InternalCmd::JumpIfFalse,
                });
                commands.push(InternalCmdDef {
                    name: format!("{}_MARKER", test),
                    kind: InternalCmd::Marker,
                });
                commands.push(InternalCmdDef {
                    name: "JUMP".to_string(),
                    kind: InternalCmd::Jump,
                });
                commands.push(InternalCmdDef {
                    name: "END_MARKER".to_string(),
                    kind: InternalCmd::Marker,
                });
            }
            ControlPattern::StartNext => {
                commands.push(InternalCmdDef {
                    name: "LOOP_SETUP".to_string(),
                    kind: InternalCmd::LoopSetup,
                });
                commands.push(InternalCmdDef {
                    name: "LOOP_NEXT".to_string(),
                    kind: InternalCmd::LoopNext,
                });
                if pattern.options.step.is_some() {
                    commands.push(InternalCmdDef {
                        name: "LOOP_STEP".to_string(),
                        kind: InternalCmd::LoopStep,
                    });
                }
            }
            ControlPattern::ForNext => {
                commands.push(InternalCmdDef {
                    name: "FOR_SETUP".to_string(),
                    kind: InternalCmd::ForSetup,
                });
                commands.push(InternalCmdDef {
                    name: "FOR_NEXT".to_string(),
                    kind: InternalCmd::ForNext,
                });
                if pattern.options.step.is_some() {
                    commands.push(InternalCmdDef {
                        name: "FOR_STEP".to_string(),
                        kind: InternalCmd::ForStep,
                    });
                }
            }
            ControlPattern::InlineConditional => {
                // IFT and IFTE are direct commands (keywords that execute directly).
                // They're already in control_keywords, so we don't add them to
                // internal_cmds to avoid duplicate constants. Their execute bodies
                // are generated separately.
            }
            ControlPattern::Case => {
                let open = pattern.keywords[0].to_string().to_uppercase();
                let end_branch = pattern.keywords[2].to_string().to_uppercase();
                let close = pattern.keywords[3].to_string().to_uppercase();
                commands.push(InternalCmdDef {
                    name: format!("{}_MARKER", open),
                    kind: InternalCmd::Marker,
                });
                commands.push(InternalCmdDef {
                    name: "JUMP_IF_FALSE".to_string(),
                    kind: InternalCmd::JumpIfFalse,
                });
                commands.push(InternalCmdDef {
                    name: "JUMP".to_string(),
                    kind: InternalCmd::Jump,
                });
                commands.push(InternalCmdDef {
                    name: format!("{}_MARKER", end_branch),
                    kind: InternalCmd::Marker,
                });
                commands.push(InternalCmdDef {
                    name: format!("{}_MARKER", close),
                    kind: InternalCmd::Marker,
                });
            }
            ControlPattern::ErrorHandler => {
                let open = pattern.keywords[0].to_string().to_uppercase();
                let handler_start = pattern.keywords[1].to_string().to_uppercase();
                let close = pattern.keywords[2].to_string().to_uppercase();
                commands.push(InternalCmdDef {
                    name: format!("{}_SETUP", open),
                    kind: InternalCmd::IfErrSetup,
                });
                commands.push(InternalCmdDef {
                    name: format!("{}_SUCCESS", open),
                    kind: InternalCmd::IfErrSuccess,
                });
                commands.push(InternalCmdDef {
                    name: "JUMP".to_string(),
                    kind: InternalCmd::Jump,
                });
                commands.push(InternalCmdDef {
                    name: format!("{}_MARKER", handler_start),
                    kind: InternalCmd::Marker,
                });
                if let Some(no_error) = &pattern.options.no_error {
                    commands.push(InternalCmdDef {
                        name: format!("{}_MARKER", no_error.to_string().to_uppercase()),
                        kind: InternalCmd::Marker,
                    });
                }
                commands.push(InternalCmdDef {
                    name: format!("{}_MARKER", close),
                    kind: InternalCmd::Marker,
                });
            }
        }
    }

    // Deduplicate by name
    commands.sort_by(|a, b| a.name.cmp(&b.name));
    commands.dedup_by(|a, b| a.name == b.name);
    commands
}

/// Generate execute body for an internal command.
fn generate_internal_cmd_execute(cmd: &InternalCmdDef) -> TokenStream {
    match cmd.kind {
        InternalCmd::Marker => {
            quote! {
                Ok(rpl_lang::library::ExecuteOk::Ok)
            }
        }
        InternalCmd::JumpIfFalse => {
            quote! {
                let target = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return Err(e.to_string()),
                };
                let condition = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return Err("Stack underflow".to_string()),
                };
                if Self::is_falsy(&condition) {
                    Ok(rpl_lang::library::ExecuteOk::Jump(target))
                } else {
                    Ok(rpl_lang::library::ExecuteOk::Ok)
                }
            }
        }
        InternalCmd::Jump => {
            quote! {
                let target = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return Err(e.to_string()),
                };
                Ok(rpl_lang::library::ExecuteOk::Jump(target))
            }
        }
        InternalCmd::UntilJump => {
            quote! {
                let target = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return Err(e.to_string()),
                };
                let condition = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return Err("Stack underflow".to_string()),
                };
                // UNTIL loops while false (exits when true)
                if Self::is_falsy(&condition) {
                    Ok(rpl_lang::library::ExecuteOk::Jump(target))
                } else {
                    Ok(rpl_lang::library::ExecuteOk::Ok)
                }
            }
        }
        InternalCmd::LoopSetup => {
            quote! {
                let finish = match ctx.pop_int() {
                    Ok(v) => v,
                    Err(_) => match ctx.pop_real() {
                        Ok(v) => v as i64,
                        Err(_) => return Err("Stack underflow".to_string()),
                    },
                };
                let start = match ctx.pop_int() {
                    Ok(v) => v,
                    Err(_) => match ctx.pop_real() {
                        Ok(v) => v as i64,
                        Err(_) => return Err("Stack underflow".to_string()),
                    },
                };
                let direction = if start < finish { -1i64 } else if start > finish { 1i64 } else { 0i64 };
                if ctx.push_start_loop(start, finish, direction, start).is_err() {
                    return Err("Return stack overflow".to_string());
                }
                Ok(rpl_lang::library::ExecuteOk::Ok)
            }
        }
        InternalCmd::LoopNext => {
            quote! {
                let target = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return Err(e.to_string()),
                };
                let (start, end, direction, counter) = match ctx.pop_start_loop() {
                    Ok(v) => v,
                    Err(_) => return Err("Return stack underflow".to_string()),
                };
                let new_counter = counter + 1;
                if new_counter <= end {
                    if ctx.push_start_loop(start, end, direction, new_counter).is_err() {
                        return Err("Return stack overflow".to_string());
                    }
                    Ok(rpl_lang::library::ExecuteOk::Jump(target))
                } else {
                    Ok(rpl_lang::library::ExecuteOk::Ok)
                }
            }
        }
        InternalCmd::LoopStep => {
            quote! {
                let target = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return Err(e.to_string()),
                };
                let increment = match ctx.pop_int() {
                    Ok(v) => v,
                    Err(_) => match ctx.pop_real() {
                        Ok(v) => v as i64,
                        Err(_) => return Err("Stack underflow: STEP requires increment".to_string()),
                    },
                };
                let (start, end, direction, counter) = match ctx.pop_start_loop() {
                    Ok(v) => v,
                    Err(_) => return Err("Return stack underflow".to_string()),
                };
                let new_counter = counter + increment;
                let should_continue = if direction < 0 {
                    new_counter <= end
                } else if direction > 0 {
                    new_counter >= end
                } else {
                    new_counter == end
                };
                if should_continue {
                    if ctx.push_start_loop(start, end, direction, new_counter).is_err() {
                        return Err("Return stack overflow".to_string());
                    }
                    Ok(rpl_lang::library::ExecuteOk::Jump(target))
                } else {
                    Ok(rpl_lang::library::ExecuteOk::Ok)
                }
            }
        }
        InternalCmd::ForSetup => {
            quote! {
                let sym_id = match ctx.read_operand() {
                    Ok(w) => w,
                    Err(e) => return Err(e.to_string()),
                };
                let finish = match ctx.pop_int() {
                    Ok(v) => v,
                    Err(_) => match ctx.pop_real() {
                        Ok(v) => v as i64,
                        Err(_) => return Err("Stack underflow".to_string()),
                    },
                };
                let start = match ctx.pop_int() {
                    Ok(v) => v,
                    Err(_) => match ctx.pop_real() {
                        Ok(v) => v as i64,
                        Err(_) => return Err("Stack underflow".to_string()),
                    },
                };
                let direction = if start < finish { -1i64 } else if start > finish { 1i64 } else { 0i64 };
                let symbol = rpl_core::Symbol::from_raw(sym_id);
                if ctx.create_local_frame_for(symbol, start).is_err() {
                    return Err("Failed to create local frame".to_string());
                }
                if ctx.push_for_loop(start, finish, direction, start).is_err() {
                    return Err("Return stack overflow".to_string());
                }
                Ok(rpl_lang::library::ExecuteOk::Ok)
            }
        }
        InternalCmd::ForNext => {
            quote! {
                let target = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return Err(e.to_string()),
                };
                let (start, end, direction, counter) = match ctx.pop_for_loop() {
                    Ok(v) => v,
                    Err(_) => return Err("Return stack underflow".to_string()),
                };
                let new_counter = counter + 1;
                if new_counter <= end {
                    ctx.set_for_variable(new_counter);
                    if ctx.push_for_loop(start, end, direction, new_counter).is_err() {
                        return Err("Return stack overflow".to_string());
                    }
                    Ok(rpl_lang::library::ExecuteOk::Jump(target))
                } else {
                    ctx.pop_local_frame();
                    Ok(rpl_lang::library::ExecuteOk::Ok)
                }
            }
        }
        InternalCmd::ForStep => {
            quote! {
                let target = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return Err(e.to_string()),
                };
                let increment = match ctx.pop_int() {
                    Ok(v) => v,
                    Err(_) => match ctx.pop_real() {
                        Ok(v) => v as i64,
                        Err(_) => return Err("Stack underflow: STEP requires increment".to_string()),
                    },
                };
                let (start, end, direction, counter) = match ctx.pop_for_loop() {
                    Ok(v) => v,
                    Err(_) => return Err("Return stack underflow".to_string()),
                };
                let new_counter = counter + increment;
                let should_continue = if direction < 0 {
                    new_counter <= end
                } else if direction > 0 {
                    new_counter >= end
                } else {
                    new_counter == end
                };
                if should_continue {
                    ctx.set_for_variable(new_counter);
                    if ctx.push_for_loop(start, end, direction, new_counter).is_err() {
                        return Err("Return stack overflow".to_string());
                    }
                    Ok(rpl_lang::library::ExecuteOk::Jump(target))
                } else {
                    ctx.pop_local_frame();
                    Ok(rpl_lang::library::ExecuteOk::Ok)
                }
            }
        }
        InternalCmd::IfErrSetup => {
            quote! {
                let handler_pc = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return Err(e.to_string()),
                };
                let code = ctx.current_code();
                if ctx.push_error_handler(handler_pc, code).is_err() {
                    return Err("Return stack overflow".to_string());
                }
                Ok(rpl_lang::library::ExecuteOk::Ok)
            }
        }
        InternalCmd::IfErrSuccess => {
            quote! {
                if ctx.pop_error_handler().is_err() {
                    return Err("No error handler to pop".to_string());
                }
                Ok(rpl_lang::library::ExecuteOk::Ok)
            }
        }
        InternalCmd::Ift => {
            quote! {
                // IFT: condition obj → obj (if true) or nothing (if false)
                let obj = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return Err("Stack underflow".to_string()),
                };
                let condition = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return Err("Stack underflow".to_string()),
                };
                if !Self::is_falsy(&condition) {
                    ctx.push(obj).map_err(|_| "Stack overflow")?;
                }
                Ok(rpl_lang::library::ExecuteOk::Ok)
            }
        }
        InternalCmd::Ifte => {
            quote! {
                // IFTE: condition true_obj false_obj → selected_obj
                let false_obj = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return Err("Stack underflow".to_string()),
                };
                let true_obj = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return Err("Stack underflow".to_string()),
                };
                let condition = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return Err("Stack underflow".to_string()),
                };
                if Self::is_falsy(&condition) {
                    ctx.push(false_obj).map_err(|_| "Stack overflow")?;
                } else {
                    ctx.push(true_obj).map_err(|_| "Stack overflow")?;
                }
                Ok(rpl_lang::library::ExecuteOk::Ok)
            }
        }
    }
}

/// Generate probe match arms for control pattern keywords.
fn generate_control_probe_arms(lib: &LibraryDef) -> Vec<TokenStream> {
    let keywords = collect_control_keywords(lib);

    keywords
        .iter()
        .filter(|(_, is_keyword)| *is_keyword)
        .map(|(kw, _)| {
            quote! {
                #kw => return rpl_lang::library::ProbeResult::Match {
                    info: rpl_core::token::TokenInfo::atom(text.len() as u8),
                    semantic: rpl_core::token::SemanticKind::Keyword,
                },
            }
        })
        .collect()
}

/// Generate compile match arms for control pattern keywords.
fn generate_control_compile_arms(lib: &LibraryDef) -> Vec<TokenStream> {
    let mut arms = Vec::new();

    for pattern in &lib.control_patterns {
        match pattern.pattern {
            ControlPattern::IfThenElse => {
                let open = pattern.keywords[0].to_string().to_uppercase();
                let test = pattern.keywords[1].to_string().to_uppercase();
                let close = pattern.keywords[2].to_string().to_uppercase();
                let open_marker = format_ident!("CMD_{}_MARKER", open);

                arms.push(quote! {
                    #open => {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::#open_marker);
                        return rpl_lang::library::CompileResult::StartConstruct {
                            kind: rpl_lang::library::ConstructKind::If,
                        }
                    }
                });
                arms.push(quote! {
                    #test => {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP_IF_FALSE);
                        ctx.emit(0); // placeholder for jump target
                        return rpl_lang::library::CompileResult::NeedMore
                    }
                });
                arms.push(quote! {
                    #close => {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_END_MARKER);
                        return rpl_lang::library::CompileResult::EndConstruct
                    }
                });

                if let Some(alt) = &pattern.options.alt {
                    let alt_upper = alt.to_string().to_uppercase();
                    arms.push(quote! {
                        #alt_upper => {
                            ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP);
                            ctx.emit(0); // placeholder
                            return rpl_lang::library::CompileResult::NeedMore
                        }
                    });
                }
            }
            ControlPattern::DoUntil => {
                let open = pattern.keywords[0].to_string().to_uppercase();
                let close = pattern.keywords[1].to_string().to_uppercase();
                let open_marker = format_ident!("CMD_{}_MARKER", open);

                arms.push(quote! {
                    #open => {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::#open_marker);
                        return rpl_lang::library::CompileResult::StartConstruct {
                            kind: rpl_lang::library::ConstructKind::DoUntil,
                        }
                    }
                });
                arms.push(quote! {
                    #close => {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_UNTIL_JUMP);
                        ctx.emit(0); // placeholder for loop-back target
                        return rpl_lang::library::CompileResult::EndConstruct
                    }
                });
            }
            ControlPattern::WhileRepeat => {
                let open = pattern.keywords[0].to_string().to_uppercase();
                let test = pattern.keywords[1].to_string().to_uppercase();
                let open_marker = format_ident!("CMD_{}_MARKER", open);
                let test_marker = format_ident!("CMD_{}_MARKER", test);

                arms.push(quote! {
                    #open => {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::#open_marker);
                        return rpl_lang::library::CompileResult::StartConstruct {
                            kind: rpl_lang::library::ConstructKind::While,
                        }
                    }
                });
                arms.push(quote! {
                    #test => {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP_IF_FALSE);
                        ctx.emit(0); // placeholder
                        ctx.emit_opcode(Self::ID.as_u16(), Self::#test_marker);
                        return rpl_lang::library::CompileResult::NeedMore
                    }
                });
                // END is shared with IfThenElse and handled there
            }
            ControlPattern::StartNext => {
                let open = pattern.keywords[0].to_string().to_uppercase();
                let close = pattern.keywords[1].to_string().to_uppercase();

                arms.push(quote! {
                    #open => {
                        return rpl_lang::library::CompileResult::StartConstruct {
                            kind: rpl_lang::library::ConstructKind::Start,
                        }
                    }
                });
                arms.push(quote! {
                    #close => {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_LOOP_NEXT);
                        ctx.emit(0); // placeholder
                        return rpl_lang::library::CompileResult::EndConstruct
                    }
                });

                if let Some(step) = &pattern.options.step {
                    let step_upper = step.to_string().to_uppercase();
                    arms.push(quote! {
                        #step_upper => {
                            ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_LOOP_STEP);
                            ctx.emit(0); // placeholder
                            return rpl_lang::library::CompileResult::EndConstruct
                        }
                    });
                }
            }
            ControlPattern::ForNext => {
                let open = pattern.keywords[0].to_string().to_uppercase();
                let close = pattern.keywords[1].to_string().to_uppercase();

                arms.push(quote! {
                    #open => {
                        return rpl_lang::library::CompileResult::StartConstruct {
                            kind: rpl_lang::library::ConstructKind::For,
                        }
                    }
                });
                arms.push(quote! {
                    #close => {
                        let cmd = match ctx.current_construct() {
                            Some(rpl_lang::library::ConstructKind::For)
                            | Some(rpl_lang::library::ConstructKind::ForUp)
                            | Some(rpl_lang::library::ConstructKind::ForDn) => Self::CMD_FOR_NEXT,
                            _ => Self::CMD_LOOP_NEXT,
                        };
                        ctx.emit_opcode(Self::ID.as_u16(), cmd);
                        ctx.emit(0);
                        return rpl_lang::library::CompileResult::EndConstruct
                    }
                });

                if let Some(step) = &pattern.options.step {
                    let step_upper = step.to_string().to_uppercase();
                    arms.push(quote! {
                        #step_upper => {
                            let cmd = match ctx.current_construct() {
                                Some(rpl_lang::library::ConstructKind::For)
                                | Some(rpl_lang::library::ConstructKind::ForUp)
                                | Some(rpl_lang::library::ConstructKind::ForDn) => Self::CMD_FOR_STEP,
                                _ => Self::CMD_LOOP_STEP,
                            };
                            ctx.emit_opcode(Self::ID.as_u16(), cmd);
                            ctx.emit(0);
                            return rpl_lang::library::CompileResult::EndConstruct
                        }
                    });
                }
            }
            ControlPattern::InlineConditional => {
                let ift = pattern.keywords[0].to_string().to_uppercase();
                let ifte = pattern.keywords[1].to_string().to_uppercase();
                let ift_cmd = format_ident!("CMD_{}", ift);
                let ifte_cmd = format_ident!("CMD_{}", ifte);

                arms.push(quote! {
                    #ift => {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::#ift_cmd);
                        return rpl_lang::library::CompileResult::Ok
                    }
                });
                arms.push(quote! {
                    #ifte => {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::#ifte_cmd);
                        return rpl_lang::library::CompileResult::Ok
                    }
                });
            }
            ControlPattern::Case => {
                let open = pattern.keywords[0].to_string().to_uppercase();
                let test = pattern.keywords[1].to_string().to_uppercase();
                let end_branch = pattern.keywords[2].to_string().to_uppercase();
                let close = pattern.keywords[3].to_string().to_uppercase();
                let open_marker = format_ident!("CMD_{}_MARKER", open);
                let end_branch_marker = format_ident!("CMD_{}_MARKER", end_branch);
                let close_marker = format_ident!("CMD_{}_MARKER", close);

                arms.push(quote! {
                    #open => {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::#open_marker);
                        return rpl_lang::library::CompileResult::StartConstruct {
                            kind: rpl_lang::library::ConstructKind::Case,
                        }
                    }
                });
                arms.push(quote! {
                    #test => {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP_IF_FALSE);
                        ctx.emit(0); // placeholder
                        return rpl_lang::library::CompileResult::NeedMore
                    }
                });
                arms.push(quote! {
                    #end_branch => {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP);
                        ctx.emit(0); // placeholder
                        ctx.emit_opcode(Self::ID.as_u16(), Self::#end_branch_marker);
                        return rpl_lang::library::CompileResult::NeedMore
                    }
                });
                arms.push(quote! {
                    #close => {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::#close_marker);
                        return rpl_lang::library::CompileResult::EndConstruct
                    }
                });
            }
            ControlPattern::ErrorHandler => {
                let open = pattern.keywords[0].to_string().to_uppercase();
                let handler_start = pattern.keywords[1].to_string().to_uppercase();
                let close = pattern.keywords[2].to_string().to_uppercase();
                let setup_cmd = format_ident!("CMD_{}_SETUP", open);
                let success_cmd = format_ident!("CMD_{}_SUCCESS", open);
                let handler_marker = format_ident!("CMD_{}_MARKER", handler_start);
                let close_marker = format_ident!("CMD_{}_MARKER", close);

                arms.push(quote! {
                    #open => {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::#setup_cmd);
                        ctx.emit(0); // placeholder for handler PC
                        return rpl_lang::library::CompileResult::StartConstruct {
                            kind: rpl_lang::library::ConstructKind::ErrorHandler,
                        }
                    }
                });
                arms.push(quote! {
                    #handler_start => {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::#success_cmd);
                        ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP);
                        ctx.emit(0); // placeholder
                        ctx.emit_opcode(Self::ID.as_u16(), Self::#handler_marker);
                        return rpl_lang::library::CompileResult::NeedMore
                    }
                });
                arms.push(quote! {
                    #close => {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::#close_marker);
                        return rpl_lang::library::CompileResult::EndConstruct
                    }
                });

                if let Some(no_error) = &pattern.options.no_error {
                    let ne_upper = no_error.to_string().to_uppercase();
                    let ne_marker = format_ident!("CMD_{}_MARKER", ne_upper);
                    arms.push(quote! {
                        #ne_upper => {
                            ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP);
                            ctx.emit(0); // placeholder
                            ctx.emit_opcode(Self::ID.as_u16(), Self::#ne_marker);
                            return rpl_lang::library::CompileResult::NeedMore
                        }
                    });
                }
            }
        }
    }

    // Deduplicate (same keyword might appear in multiple patterns)
    // For now, we just return all arms and rely on match ordering
    arms
}

/// Generate stack effect match arms for control pattern keywords.
fn generate_control_stack_effect_arms(lib: &LibraryDef) -> Vec<TokenStream> {
    let mut arms = Vec::new();

    for pattern in &lib.control_patterns {
        match pattern.pattern {
            ControlPattern::IfThenElse => {
                let open = pattern.keywords[0].to_string().to_uppercase();
                let test = pattern.keywords[1].to_string().to_uppercase();
                let close = pattern.keywords[2].to_string().to_uppercase();

                arms.push(quote! { #open => rpl_lang::library::StackEffect::StartConstruct, });
                arms.push(quote! { #test => rpl_lang::library::StackEffect::Fixed { consumes: 1, produces: 0 }, });
                arms.push(quote! { #close => rpl_lang::library::StackEffect::EndConstruct, });

                if let Some(alt) = &pattern.options.alt {
                    let alt_upper = alt.to_string().to_uppercase();
                    arms.push(quote! { #alt_upper => rpl_lang::library::StackEffect::Fixed { consumes: 0, produces: 0 }, });
                }
            }
            ControlPattern::DoUntil => {
                let open = pattern.keywords[0].to_string().to_uppercase();
                let close = pattern.keywords[1].to_string().to_uppercase();

                arms.push(quote! { #open => rpl_lang::library::StackEffect::StartConstruct, });
                arms.push(quote! { #close => rpl_lang::library::StackEffect::Fixed { consumes: 1, produces: 0 }, });
            }
            ControlPattern::WhileRepeat => {
                let open = pattern.keywords[0].to_string().to_uppercase();
                let test = pattern.keywords[1].to_string().to_uppercase();

                arms.push(quote! { #open => rpl_lang::library::StackEffect::StartConstruct, });
                arms.push(quote! { #test => rpl_lang::library::StackEffect::Fixed { consumes: 1, produces: 0 }, });
                // END is shared
            }
            ControlPattern::StartNext => {
                let open = pattern.keywords[0].to_string().to_uppercase();
                let close = pattern.keywords[1].to_string().to_uppercase();

                arms.push(quote! { #open => rpl_lang::library::StackEffect::Fixed { consumes: 2, produces: 0 }, });
                arms.push(quote! { #close => rpl_lang::library::StackEffect::Fixed { consumes: 0, produces: 0 }, });

                if let Some(step) = &pattern.options.step {
                    let step_upper = step.to_string().to_uppercase();
                    arms.push(quote! { #step_upper => rpl_lang::library::StackEffect::Fixed { consumes: 1, produces: 0 }, });
                }
            }
            ControlPattern::ForNext => {
                let open = pattern.keywords[0].to_string().to_uppercase();
                let close = pattern.keywords[1].to_string().to_uppercase();

                arms.push(quote! { #open => rpl_lang::library::StackEffect::Fixed { consumes: 2, produces: 0 }, });
                arms.push(quote! { #close => rpl_lang::library::StackEffect::Fixed { consumes: 0, produces: 0 }, });

                if let Some(step) = &pattern.options.step {
                    let step_upper = step.to_string().to_uppercase();
                    arms.push(quote! { #step_upper => rpl_lang::library::StackEffect::Fixed { consumes: 1, produces: 0 }, });
                }
            }
            ControlPattern::InlineConditional => {
                let ift = pattern.keywords[0].to_string().to_uppercase();
                let ifte = pattern.keywords[1].to_string().to_uppercase();

                arms.push(quote! { #ift => rpl_lang::library::StackEffect::Fixed { consumes: 2, produces: 1 }, });
                arms.push(quote! { #ifte => rpl_lang::library::StackEffect::Fixed { consumes: 3, produces: 1 }, });
            }
            ControlPattern::Case => {
                let open = pattern.keywords[0].to_string().to_uppercase();
                let test = pattern.keywords[1].to_string().to_uppercase();
                let end_branch = pattern.keywords[2].to_string().to_uppercase();
                let close = pattern.keywords[3].to_string().to_uppercase();

                arms.push(quote! { #open => rpl_lang::library::StackEffect::StartConstruct, });
                arms.push(quote! { #test => rpl_lang::library::StackEffect::Fixed { consumes: 1, produces: 0 }, });
                arms.push(quote! { #end_branch => rpl_lang::library::StackEffect::Fixed { consumes: 0, produces: 0 }, });
                arms.push(quote! { #close => rpl_lang::library::StackEffect::EndConstruct, });
            }
            ControlPattern::ErrorHandler => {
                let open = pattern.keywords[0].to_string().to_uppercase();
                let handler_start = pattern.keywords[1].to_string().to_uppercase();
                let close = pattern.keywords[2].to_string().to_uppercase();

                arms.push(quote! { #open => rpl_lang::library::StackEffect::StartConstruct, });
                arms.push(quote! { #handler_start => rpl_lang::library::StackEffect::Fixed { consumes: 0, produces: 0 }, });
                arms.push(quote! { #close => rpl_lang::library::StackEffect::EndConstruct, });

                if let Some(no_error) = &pattern.options.no_error {
                    let ne_upper = no_error.to_string().to_uppercase();
                    arms.push(quote! { #ne_upper => rpl_lang::library::StackEffect::Fixed { consumes: 0, produces: 0 }, });
                }
            }
        }
    }

    arms
}

/// Check if library has control patterns.
fn has_control_patterns(lib: &LibraryDef) -> bool {
    !lib.control_patterns.is_empty()
}

/// Get the keyword name an internal command should decompile to.
/// Returns None if the command shouldn't appear in decompiled output (e.g., pure jumps).
fn internal_cmd_decompile_name(cmd: &InternalCmdDef) -> Option<String> {
    match cmd.kind {
        InternalCmd::Marker => {
            // Extract keyword from marker name: "IF_MARKER" -> "IF"
            cmd.name.strip_suffix("_MARKER").map(|s| s.to_string())
        }
        InternalCmd::JumpIfFalse => Some("THEN".to_string()), // Default context
        InternalCmd::Jump => None, // Invisible in output (part of ELSE handling)
        InternalCmd::UntilJump => Some("UNTIL".to_string()),
        InternalCmd::LoopSetup => Some("START".to_string()),
        InternalCmd::LoopNext => Some("NEXT".to_string()),
        InternalCmd::LoopStep => Some("STEP".to_string()),
        InternalCmd::ForSetup => Some("FOR".to_string()),
        InternalCmd::ForNext => Some("NEXT".to_string()),
        InternalCmd::ForStep => Some("STEP".to_string()),
        InternalCmd::IfErrSetup => Some("IFERR".to_string()),
        InternalCmd::IfErrSuccess => None, // Part of THEN handling
        InternalCmd::Ift => Some("IFT".to_string()),
        InternalCmd::Ifte => Some("IFTE".to_string()),
    }
}

/// Check if an internal command has an operand that needs to be read during decompilation.
fn internal_cmd_has_operand(cmd: &InternalCmdDef) -> bool {
    matches!(
        cmd.kind,
        InternalCmd::JumpIfFalse
            | InternalCmd::Jump
            | InternalCmd::UntilJump
            | InternalCmd::LoopNext
            | InternalCmd::LoopStep
            | InternalCmd::ForSetup
            | InternalCmd::ForNext
            | InternalCmd::ForStep
            | InternalCmd::IfErrSetup
    )
}

/// Generate decompile match arms for internal commands with operands.
/// These need special handling to skip their operands during decompilation.
fn generate_control_decompile_arms(lib: &LibraryDef) -> Vec<TokenStream> {
    let internal_cmds = collect_control_internal_commands(lib);

    internal_cmds
        .iter()
        .filter(|cmd| internal_cmd_has_operand(cmd))
        .map(|cmd| {
            let const_name = format_ident!("CMD_{}", cmd.name);
            let decompile_name = internal_cmd_decompile_name(cmd);

            match decompile_name {
                Some(name) => {
                    quote! {
                        Self::#const_name => {
                            ctx.read(); // skip operand
                            ctx.write(#name);
                            return rpl_lang::library::DecompileResult::Ok;
                        }
                    }
                }
                None => {
                    // Command is invisible (like JUMP) - just skip operand
                    quote! {
                        Self::#const_name => {
                            ctx.read(); // skip operand
                            return rpl_lang::library::DecompileResult::Ok;
                        }
                    }
                }
            }
        })
        .collect()
}

/// Generate the is_falsy helper function if control patterns are present.
fn generate_is_falsy_helper(lib: &LibraryDef) -> TokenStream {
    if !has_control_patterns(lib) {
        return quote! {};
    }

    quote! {
        /// Check if a value is falsy (0, 0.0, or false).
        fn is_falsy(value: &rpl_lang::Value) -> bool {
            match value {
                rpl_lang::Value::Real(r) => *r == 0.0,
                rpl_lang::Value::Int(i) => *i == 0,
                rpl_lang::Value::Bool(b) => !b,
                _ => false, // Non-numeric values are truthy
            }
        }
    }
}
