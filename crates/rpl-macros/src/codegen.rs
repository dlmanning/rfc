//! Code generation for the define_library! macro.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::ast::{Arity, ConstructAction, ControlCloser, LibraryDef, OpSigDef, StackEffectDef};

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
    // Get emit commands from control flow declarations
    let emit_cmds = collect_control_emit_commands(lib);
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

    // Generate command constants for emit commands (after regular commands)
    let emit_cmd_consts: Vec<_> = emit_cmds
        .iter()
        .enumerate()
        .map(|(i, cmd)| {
            let const_name = format_ident!("CMD_{}", cmd);
            let idx = num_regular_commands + i as u16;
            quote! {
                const #const_name: u16 = #idx;
            }
        })
        .collect();

    // Generate command constants for control keywords (after emit commands)
    // Filter out keywords that are already emit commands to avoid duplicates
    let emit_cmd_names: std::collections::HashSet<&str> =
        emit_cmds.iter().map(|cmd| cmd.as_str()).collect();
    let num_emit_commands = emit_cmds.len() as u16;
    let filtered_control_keywords: Vec<_> = control_keywords
        .iter()
        .filter(|(kw, _)| !emit_cmd_names.contains(kw.as_str()))
        .collect();
    let control_cmd_consts: Vec<_> = filtered_control_keywords
        .iter()
        .enumerate()
        .map(|(i, (kw, _))| {
            let const_name = format_ident!("CMD_{}", kw);
            let idx = num_regular_commands + num_emit_commands + i as u16;
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

    // Generate command_name match arms for emit commands
    // Emit commands display as their name
    let emit_name_arms: Vec<_> = emit_cmds
        .iter()
        .map(|cmd| {
            let const_name = format_ident!("CMD_{}", cmd);
            quote! {
                Self::#const_name => Some(#cmd),
            }
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
            #(#emit_cmd_consts)*
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
                    #(#emit_name_arms)*
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
        let (symbols, words): (Vec<_>, Vec<_>) = op
            .tokens
            .iter()
            .partition(|t| !t.chars().all(|c| c.is_alphabetic()));

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
    if !lib.operator_syntax.is_empty() && lib.commands.is_empty() && !has_control_flow(lib) {
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

    // Generate execute arms for inline control flow keywords (IFT, IFTE)
    let inline_arms: Vec<_> = lib
        .control_flow
        .inlines
        .iter()
        .map(|inline| {
            let kw = inline.keyword.to_string().to_uppercase();
            let const_name = format_ident!("CMD_{}", kw);

            // For IFT and IFTE, generate special handling
            // TODO: In the future, we could allow custom execute bodies in the DSL
            if kw == "IFT" {
                quote! {
                    Self::#const_name => {
                        // IFT: obj cond → obj (if true) or nothing (if false)
                        let condition = match ctx.pop() {
                            Ok(v) => v,
                            Err(_) => return Err("Stack underflow".to_string()),
                        };
                        let obj = match ctx.pop() {
                            Ok(v) => v,
                            Err(_) => return Err("Stack underflow".to_string()),
                        };
                        if !Self::is_falsy(&condition) {
                            ctx.push(obj).map_err(|_| "Stack overflow")?;
                        }
                        Ok(rpl_lang::library::ExecuteOk::Ok)
                    }
                }
            } else if kw == "IFTE" {
                quote! {
                    Self::#const_name => {
                        // IFTE: true_obj false_obj cond → selected_obj
                        let condition = match ctx.pop() {
                            Ok(v) => v,
                            Err(_) => return Err("Stack underflow".to_string()),
                        };
                        let false_obj = match ctx.pop() {
                            Ok(v) => v,
                            Err(_) => return Err("Stack underflow".to_string()),
                        };
                        let true_obj = match ctx.pop() {
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
            } else {
                // Unknown inline - should be handled in commands block
                quote! {}
            }
        })
        .filter(|ts| !ts.is_empty())
        .collect();

    quote! {
        fn execute(&self, ctx: &mut rpl_lang::library::ExecuteContext) -> rpl_lang::library::ExecuteResult {
            match ctx.cmd() {
                #(#arms)*
                #(#inline_arms)*
                _ => Err(format!("Unknown {} command: {}", #lib_name_str, ctx.cmd())),
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
// Control Flow Code Generation
// =============================================================================
// The new control_flow DSL uses explicit role-based declarations:
// - opener: Starts a construct
// - transition: Emits bytecode, stays in construct (NeedMore)
// - closer: Ends a construct
// - inline: Regular commands with stack effects

/// Check if library has control flow declarations.
fn has_control_flow(lib: &LibraryDef) -> bool {
    let cf = &lib.control_flow;
    !cf.openers.is_empty()
        || !cf.transitions.is_empty()
        || !cf.closers.is_empty()
        || !cf.inlines.is_empty()
}

/// Collect all keywords from control flow declarations.
/// Returns a list of (keyword_name, is_keyword) pairs.
fn collect_control_keywords(lib: &LibraryDef) -> Vec<(String, bool)> {
    let mut keywords = Vec::new();
    let cf = &lib.control_flow;

    for opener in &cf.openers {
        keywords.push((opener.keyword.to_string().to_uppercase(), true));
    }
    for transition in &cf.transitions {
        keywords.push((transition.keyword.to_string().to_uppercase(), true));
    }
    for closer in &cf.closers {
        keywords.push((closer.keyword.to_string().to_uppercase(), true));
    }
    for inline in &cf.inlines {
        keywords.push((inline.keyword.to_string().to_uppercase(), true));
    }

    // Deduplicate
    keywords.sort_by(|a, b| a.0.cmp(&b.0));
    keywords.dedup_by(|a, b| a.0 == b.0);
    keywords
}

/// Collect all emit commands referenced in control flow declarations.
/// These become internal commands that need CMD_* constants.
fn collect_control_emit_commands(lib: &LibraryDef) -> Vec<String> {
    let mut cmds = Vec::new();
    let cf = &lib.control_flow;

    // Collect names of commands already defined in the commands block
    let existing_cmd_names: std::collections::HashSet<String> = lib
        .commands
        .iter()
        .map(|cmd| cmd.name.to_string().to_uppercase())
        .collect();

    for opener in &cf.openers {
        for cmd in &opener.emit {
            cmds.push(cmd.to_string().to_uppercase());
        }
    }
    for transition in &cf.transitions {
        for cmd in &transition.emit {
            cmds.push(cmd.to_string().to_uppercase());
        }
    }
    for closer in &cf.closers {
        for cmd in &closer.emit {
            cmds.push(cmd.to_string().to_uppercase());
        }
    }

    // Deduplicate
    cmds.sort();
    cmds.dedup();

    // Filter out commands already defined in commands block
    cmds.into_iter()
        .filter(|cmd| !existing_cmd_names.contains(cmd))
        .collect()
}

/// Generate probe match arms for control flow keywords.
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

/// Generate compile match arms for control flow keywords.
fn generate_control_compile_arms(lib: &LibraryDef) -> Vec<TokenStream> {
    let mut arms = Vec::new();
    let cf = &lib.control_flow;

    // Openers - return StartConstruct, optionally emit setup commands
    for opener in &cf.openers {
        let kw = opener.keyword.to_string().to_uppercase();
        let construct = &opener.construct;

        let emit_code = if opener.emit.is_empty() {
            quote! {}
        } else {
            let emit_stmts: Vec<_> = opener
                .emit
                .iter()
                .map(|cmd| {
                    let cmd_const = format_ident!("CMD_{}", cmd.to_string().to_uppercase());
                    quote! {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::#cmd_const);
                        ctx.emit(0); // placeholder
                    }
                })
                .collect();
            quote! { #(#emit_stmts)* }
        };

        arms.push(quote! {
            #kw => {
                #emit_code
                return rpl_lang::library::CompileResult::StartConstruct {
                    kind: rpl_lang::library::ConstructKind::#construct,
                }
            }
        });
    }

    // Transitions - emit commands and return NeedMore
    // NOTE: Only JUMP commands need placeholders (for offset). Other commands don't.
    for transition in &cf.transitions {
        let kw = transition.keyword.to_string().to_uppercase();

        let emit_stmts: Vec<_> = transition
            .emit
            .iter()
            .map(|cmd| {
                let cmd_name = cmd.to_string().to_uppercase();
                let cmd_const = format_ident!("CMD_{}", cmd_name);
                // Only emit placeholder for JUMP-type commands
                let needs_placeholder = cmd_name.contains("JUMP");
                if needs_placeholder {
                    quote! {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::#cmd_const);
                        ctx.emit(0); // placeholder for jump target
                    }
                } else {
                    quote! {
                        ctx.emit_opcode(Self::ID.as_u16(), Self::#cmd_const);
                    }
                }
            })
            .collect();

        // If valid_in is specified, check current_construct
        if transition.valid_in.is_empty() {
            arms.push(quote! {
                #kw => {
                    #(#emit_stmts)*
                    return rpl_lang::library::CompileResult::NeedMore
                }
            });
        } else {
            let kinds: Vec<_> = transition
                .valid_in
                .iter()
                .map(|k| {
                    quote! { Some(rpl_lang::library::ConstructKind::#k) }
                })
                .collect();
            // IMPORTANT: Return Unknown if construct doesn't match to prevent
            // falling through to default command_id handling
            arms.push(quote! {
                #kw => {
                    match ctx.current_construct() {
                        #(#kinds)|* => {
                            #(#emit_stmts)*
                            return rpl_lang::library::CompileResult::NeedMore
                        }
                        _ => return rpl_lang::library::CompileResult::NoMatch
                    }
                }
            });
        }
    }

    // Closers - may emit commands, return EndConstruct
    // Group closers by keyword to handle shared keywords (like NEXT)
    let mut closer_map: std::collections::HashMap<String, Vec<&ControlCloser>> =
        std::collections::HashMap::new();
    for closer in &cf.closers {
        let kw = closer.keyword.to_string().to_uppercase();
        closer_map.entry(kw).or_default().push(closer);
    }

    for (kw, closers) in &closer_map {
        if closers.len() == 1 && closers[0].emit.is_empty() {
            // Simple case: single closer, no emit
            let kinds: Vec<_> = closers[0]
                .valid_in
                .iter()
                .map(|k| {
                    quote! { Some(rpl_lang::library::ConstructKind::#k) }
                })
                .collect();
            arms.push(quote! {
                #kw => {
                    match ctx.current_construct() {
                        #(#kinds)|* => return rpl_lang::library::CompileResult::EndConstruct,
                        _ => return rpl_lang::library::CompileResult::NoMatch
                    }
                }
            });
        } else if closers.len() == 1 {
            // Single closer with emit
            let closer = closers[0];
            let kinds: Vec<_> = closer
                .valid_in
                .iter()
                .map(|k| {
                    quote! { Some(rpl_lang::library::ConstructKind::#k) }
                })
                .collect();
            let emit_stmts: Vec<_> = closer
                .emit
                .iter()
                .map(|cmd| {
                    let cmd_name = cmd.to_string().to_uppercase();
                    let cmd_const = format_ident!("CMD_{}", cmd_name);
                    // Only JUMP and LOOP/FOR commands need placeholders
                    let needs_placeholder = cmd_name.contains("JUMP")
                        || cmd_name.contains("LOOP_NEXT")
                        || cmd_name.contains("LOOP_STEP")
                        || cmd_name.contains("FOR_NEXT")
                        || cmd_name.contains("FOR_STEP");
                    if needs_placeholder {
                        quote! {
                            ctx.emit_opcode(Self::ID.as_u16(), Self::#cmd_const);
                            ctx.emit(0); // placeholder
                        }
                    } else {
                        quote! {
                            ctx.emit_opcode(Self::ID.as_u16(), Self::#cmd_const);
                        }
                    }
                })
                .collect();
            arms.push(quote! {
                #kw => {
                    match ctx.current_construct() {
                        #(#kinds)|* => {
                            #(#emit_stmts)*
                            return rpl_lang::library::CompileResult::EndConstruct
                        }
                        _ => return rpl_lang::library::CompileResult::NoMatch
                    }
                }
            });
        } else {
            // Multiple closers for same keyword (e.g., NEXT for Start vs For)
            // Generate match arms for each
            let mut match_arms = Vec::new();
            for closer in closers {
                let kinds: Vec<_> = closer
                    .valid_in
                    .iter()
                    .map(|k| {
                        quote! { Some(rpl_lang::library::ConstructKind::#k) }
                    })
                    .collect();

                if closer.emit.is_empty() {
                    match_arms.push(quote! {
                        #(#kinds)|* => return rpl_lang::library::CompileResult::EndConstruct,
                    });
                } else {
                    let emit_stmts: Vec<_> = closer
                        .emit
                        .iter()
                        .map(|cmd| {
                            let cmd_name = cmd.to_string().to_uppercase();
                            let cmd_const = format_ident!("CMD_{}", cmd_name);
                            // Only JUMP and LOOP/FOR commands need placeholders
                            let needs_placeholder = cmd_name.contains("JUMP")
                                || cmd_name.contains("LOOP_NEXT")
                                || cmd_name.contains("LOOP_STEP")
                                || cmd_name.contains("FOR_NEXT")
                                || cmd_name.contains("FOR_STEP");
                            if needs_placeholder {
                                quote! {
                                    ctx.emit_opcode(Self::ID.as_u16(), Self::#cmd_const);
                                    ctx.emit(0);
                                }
                            } else {
                                quote! {
                                    ctx.emit_opcode(Self::ID.as_u16(), Self::#cmd_const);
                                }
                            }
                        })
                        .collect();
                    match_arms.push(quote! {
                        #(#kinds)|* => {
                            #(#emit_stmts)*
                            return rpl_lang::library::CompileResult::EndConstruct
                        }
                    });
                }
            }
            // Add fallthrough to NoMatch for unmatched constructs
            arms.push(quote! {
                #kw => {
                    match ctx.current_construct() {
                        #(#match_arms)*
                        _ => return rpl_lang::library::CompileResult::NoMatch
                    }
                }
            });
        }
    }

    // Inlines - just emit command and return Ok
    for inline in &cf.inlines {
        let kw = inline.keyword.to_string().to_uppercase();
        let cmd_const = format_ident!("CMD_{}", kw);
        arms.push(quote! {
            #kw => {
                ctx.emit_opcode(Self::ID.as_u16(), Self::#cmd_const);
                return rpl_lang::library::CompileResult::Ok
            }
        });
    }

    arms
}

/// Generate stack effect match arms for control flow keywords.
fn generate_control_stack_effect_arms(lib: &LibraryDef) -> Vec<TokenStream> {
    let mut arms = Vec::new();
    let cf = &lib.control_flow;

    // Openers
    for opener in &cf.openers {
        let kw = opener.keyword.to_string().to_uppercase();
        if let Some(StackEffectDef::Fixed { consumes, produces }) = &opener.effect {
            arms.push(quote! { #kw => rpl_lang::library::StackEffect::Fixed { consumes: #consumes, produces: #produces }, });
        } else {
            // Default: StartConstruct
            arms.push(quote! { #kw => rpl_lang::library::StackEffect::StartConstruct, });
        }
    }

    // Transitions
    for transition in &cf.transitions {
        let kw = transition.keyword.to_string().to_uppercase();
        if let Some(StackEffectDef::Fixed { consumes, produces }) = &transition.effect {
            arms.push(quote! { #kw => rpl_lang::library::StackEffect::Fixed { consumes: #consumes, produces: #produces }, });
        } else {
            arms.push(quote! { #kw => rpl_lang::library::StackEffect::Fixed { consumes: 0, produces: 0 }, });
        }
    }

    // Closers - deduplicate by keyword
    let mut seen_closers: std::collections::HashSet<String> = std::collections::HashSet::new();
    for closer in &cf.closers {
        let kw = closer.keyword.to_string().to_uppercase();
        if seen_closers.contains(&kw) {
            continue;
        }
        seen_closers.insert(kw.clone());

        if let Some(StackEffectDef::Fixed { consumes, produces }) = &closer.effect {
            arms.push(quote! { #kw => rpl_lang::library::StackEffect::Fixed { consumes: #consumes, produces: #produces }, });
        } else {
            // Default for closers is EndConstruct or (0 -> 0)
            // Check if any closer for this keyword has an effect
            let any_has_emit = cf
                .closers
                .iter()
                .any(|c| c.keyword.to_string().to_uppercase() == kw && !c.emit.is_empty());
            if any_has_emit {
                arms.push(quote! { #kw => rpl_lang::library::StackEffect::Fixed { consumes: 0, produces: 0 }, });
            } else {
                arms.push(quote! { #kw => rpl_lang::library::StackEffect::EndConstruct, });
            }
        }
    }

    // Inlines
    for inline in &cf.inlines {
        let kw = inline.keyword.to_string().to_uppercase();
        if let StackEffectDef::Fixed { consumes, produces } = &inline.effect {
            arms.push(quote! { #kw => rpl_lang::library::StackEffect::Fixed { consumes: #consumes, produces: #produces }, });
        }
    }

    arms
}

/// Generate the is_falsy helper function if control flow is present.
fn generate_is_falsy_helper(lib: &LibraryDef) -> TokenStream {
    if !has_control_flow(lib) {
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
