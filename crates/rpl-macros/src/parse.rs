//! Parsing for the define_library! macro.

use proc_macro2::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{braced, Ident, LitInt, LitStr, Path, Result, Token};

use crate::ast::{
    Arity, CoercionDef, CommandDef, ConstructAction, ConstructDef, ControlPattern,
    ControlPatternDef, ControlPatternOptions, DocDef, LibraryDef, LiteralDef, OpSigDef,
    OperatorDef, OperatorSyntaxDef, PrologDef, PrologKind, StackEffectDef,
};

mod kw {
    // Library header
    syn::custom_keyword!(library);
    syn::custom_keyword!(id);
    syn::custom_keyword!(name);

    // Section keywords
    syn::custom_keyword!(literals);
    syn::custom_keyword!(commands);
    syn::custom_keyword!(constructs);
    syn::custom_keyword!(operators);
    syn::custom_keyword!(syntax);           // NEW: renamed from operator_syntax
    syn::custom_keyword!(operator_syntax);  // COMPAT: keep old name
    syn::custom_keyword!(prologs);
    syn::custom_keyword!(control_patterns);
    syn::custom_keyword!(coercions);
    syn::custom_keyword!(custom);

    // Custom handler keywords
    syn::custom_keyword!(probe);
    syn::custom_keyword!(compile);
    syn::custom_keyword!(decompile_prolog);
    syn::custom_keyword!(decompile);
    syn::custom_keyword!(stack_effect);

    // Command keywords
    syn::custom_keyword!(dynamic);          // COMPAT: old dynamic keyword
    syn::custom_keyword!(brief);
    syn::custom_keyword!(stack);
    syn::custom_keyword!(example);
    syn::custom_keyword!(see_also);
    syn::custom_keyword!(alias);            // NEW: for command alias in options block

    // Construct keywords
    syn::custom_keyword!(open);
    syn::custom_keyword!(close);

    // Prolog keywords
    syn::custom_keyword!(delimited);
    syn::custom_keyword!(format);           // NEW: renamed from format_codec
    syn::custom_keyword!(format_codec);     // COMPAT: keep old name

    // Operator keywords
    syn::custom_keyword!(Symmetric);
    syn::custom_keyword!(Unary);
    syn::custom_keyword!(Binary);
    syn::custom_keyword!(commutative);
    syn::custom_keyword!(result);
    syn::custom_keyword!(priority);
    syn::custom_keyword!(infix);
    syn::custom_keyword!(right_assoc);
    syn::custom_keyword!(unary);
    syn::custom_keyword!(binary);

    // Control pattern keywords
    syn::custom_keyword!(if_then_else);
    syn::custom_keyword!(do_until);
    syn::custom_keyword!(while_repeat);
    syn::custom_keyword!(start_next);
    syn::custom_keyword!(for_next);
    syn::custom_keyword!(inline_conditional);
    syn::custom_keyword!(case_pattern);     // COMPAT: old name
    syn::custom_keyword!(error_handler);

    // Control pattern option keywords
    syn::custom_keyword!(alt);              // NEW: for if_then_else ELSE
    syn::custom_keyword!(step);             // NEW: for start_next STEP
    syn::custom_keyword!(no_error);

    // Coercion keywords
    syn::custom_keyword!(implicit);
}

impl Parse for LibraryDef {
    fn parse(input: ParseStream) -> Result<Self> {
        // Parse visibility
        let vis: syn::Visibility = input.parse()?;

        // NEW syntax: pub library Name(id, "display_name");
        // COMPAT: pub struct Name { id: N, name: "..." }
        let (name, id, display_name) = if input.peek(kw::library) {
            input.parse::<kw::library>()?;
            let name: Ident = input.parse()?;

            let args;
            syn::parenthesized!(args in input);
            let id: LitInt = args.parse()?;
            args.parse::<Token![,]>()?;
            let display_name: LitStr = args.parse()?;

            input.parse::<Token![;]>()?;

            (name, id, display_name)
        } else {
            // Old syntax: struct Name { id: N, name: "..." }
            input.parse::<Token![struct]>()?;
            let name: Ident = input.parse()?;

            let content;
            braced!(content in input);

            // Parse id: N,
            content.parse::<kw::id>()?;
            content.parse::<Token![:]>()?;
            let id: LitInt = content.parse()?;
            content.parse::<Token![,]>()?;

            // Parse name: "...",
            content.parse::<kw::name>()?;
            content.parse::<Token![:]>()?;
            let display_name: LitStr = content.parse()?;
            // Optional trailing comma
            let _ = content.parse::<Token![,]>();

            (name, id, display_name)
        };

        let mut literals = Vec::new();
        let mut commands = Vec::new();
        let mut constructs = Vec::new();
        let mut operators = Vec::new();
        let mut operator_syntax = Vec::new();
        let mut prologs = Vec::new();
        let mut control_patterns = Vec::new();
        let mut coercions = Vec::new();
        let mut custom_probe = None;
        let mut custom_compile = None;
        let mut custom_decompile_prolog = None;
        let mut custom_decompile = None;
        let mut custom_stack_effect = None;

        // Parse remaining blocks: literals { ... }, commands { ... }, constructs { ... }, operators { ... }, custom probe { ... }, etc.
        while !input.is_empty() {
            if input.peek(kw::literals) {
                input.parse::<kw::literals>()?;
                let lits_content;
                braced!(lits_content in input);

                while !lits_content.is_empty() {
                    literals.push(parse_literal(&lits_content)?);
                }
            } else if input.peek(kw::commands) {
                input.parse::<kw::commands>()?;
                let cmds_content;
                braced!(cmds_content in input);

                while !cmds_content.is_empty() {
                    commands.push(cmds_content.parse()?);
                }
            } else if input.peek(kw::constructs) {
                input.parse::<kw::constructs>()?;
                let constructs_content;
                braced!(constructs_content in input);

                while !constructs_content.is_empty() {
                    constructs.push(parse_construct(&constructs_content)?);
                }
            } else if input.peek(kw::operators) {
                input.parse::<kw::operators>()?;
                let ops_content;
                braced!(ops_content in input);

                while !ops_content.is_empty() {
                    operators.push(parse_operator(&ops_content)?);
                }
            } else if input.peek(kw::operator_syntax) {
                input.parse::<kw::operator_syntax>()?;
                let ops_content;
                braced!(ops_content in input);

                while !ops_content.is_empty() {
                    operator_syntax.push(parse_operator_syntax(&ops_content)?);
                }
            } else if input.peek(kw::prologs) {
                input.parse::<kw::prologs>()?;
                let prologs_content;
                braced!(prologs_content in input);

                while !prologs_content.is_empty() {
                    prologs.push(parse_prolog(&prologs_content)?);
                }
            } else if input.peek(kw::control_patterns) {
                input.parse::<kw::control_patterns>()?;
                let patterns_content;
                braced!(patterns_content in input);

                while !patterns_content.is_empty() {
                    control_patterns.push(parse_control_pattern(&patterns_content)?);
                }
            } else if input.peek(kw::coercions) {
                input.parse::<kw::coercions>()?;
                let coercions_content;
                braced!(coercions_content in input);

                while !coercions_content.is_empty() {
                    coercions.push(parse_coercion(&coercions_content)?);
                }
            } else if input.peek(kw::custom) {
                input.parse::<kw::custom>()?;

                if input.peek(kw::probe) {
                    input.parse::<kw::probe>()?;
                    // Optionally parse (ctx) - we ignore it since ctx is always available
                    if input.peek(syn::token::Paren) {
                        let args_content;
                        syn::parenthesized!(args_content in input);
                        let _: Ident = args_content.parse()?; // consume ctx
                    }
                    let body_content;
                    braced!(body_content in input);
                    custom_probe = Some(body_content.parse()?);
                } else if input.peek(kw::compile) {
                    input.parse::<kw::compile>()?;
                    // Optionally parse (ctx) - we ignore it since ctx is always available
                    if input.peek(syn::token::Paren) {
                        let args_content;
                        syn::parenthesized!(args_content in input);
                        let _: Ident = args_content.parse()?; // consume ctx
                    }
                    let body_content;
                    braced!(body_content in input);
                    custom_compile = Some(body_content.parse()?);
                } else if input.peek(kw::decompile_prolog) {
                    input.parse::<kw::decompile_prolog>()?;
                    // Optionally parse (ctx) - we ignore it since ctx is always available
                    if input.peek(syn::token::Paren) {
                        let args_content;
                        syn::parenthesized!(args_content in input);
                        let _: Ident = args_content.parse()?; // consume ctx
                    }
                    let body_content;
                    braced!(body_content in input);
                    custom_decompile_prolog = Some(body_content.parse()?);
                } else if input.peek(kw::decompile) {
                    input.parse::<kw::decompile>()?;
                    // Optionally parse (ctx) - we ignore it since ctx is always available
                    if input.peek(syn::token::Paren) {
                        let args_content;
                        syn::parenthesized!(args_content in input);
                        let _: Ident = args_content.parse()?; // consume ctx
                    }
                    let body_content;
                    braced!(body_content in input);
                    custom_decompile = Some(body_content.parse()?);
                } else if input.peek(kw::stack_effect) {
                    input.parse::<kw::stack_effect>()?;
                    // Optionally parse (token) - we ignore it since token is always available
                    if input.peek(syn::token::Paren) {
                        let args_content;
                        syn::parenthesized!(args_content in input);
                        let _: Ident = args_content.parse()?; // consume token
                    }
                    let body_content;
                    braced!(body_content in input);
                    custom_stack_effect = Some(body_content.parse()?);
                } else {
                    return Err(input.error("expected 'probe', 'compile', 'decompile_prolog', 'decompile', or 'stack_effect' after 'custom'"));
                }
            } else {
                return Err(input.error("expected 'literals', 'commands', 'constructs', 'operators', 'operator_syntax', 'prologs', 'control_patterns', 'coercions', or 'custom'"));
            }
        }

        Ok(LibraryDef {
            vis,
            name,
            id,
            display_name,
            literals,
            commands,
            constructs,
            operators,
            operator_syntax,
            prologs,
            control_patterns,
            coercions,
            custom_probe,
            custom_compile,
            custom_decompile_prolog,
            custom_decompile,
            custom_stack_effect,
        })
    }
}

impl Parse for CommandDef {
    fn parse(input: ParseStream) -> Result<Self> {
        // Check for @ prefix (internal command)
        let internal = input.peek(Token![@]);
        if internal {
            input.parse::<Token![@]>()?;
        }

        // Parse command name
        let name: Ident = input.parse()?;

        // Parse optional aliases: | "alias1" | "alias2"
        let mut aliases = Vec::new();
        while input.peek(Token![|]) {
            input.parse::<Token![|]>()?;
            let alias: LitStr = input.parse()?;
            aliases.push(alias.value());
        }

        // Parse stack effect: (N -> M) or (*)
        // NEW syntax: NAME(N -> M) or NAME(*) without =>
        // COMPAT: also accept NAME => ... and `dynamic` keyword for migration
        if input.peek(Token![=>]) {
            input.parse::<Token![=>]>()?;
        }

        let effect = if input.peek(kw::dynamic) {
            // COMPAT: old `dynamic` keyword
            input.parse::<kw::dynamic>()?;
            StackEffectDef::Dynamic
        } else {
            // Parse (N -> M) or (*)
            let effect_content;
            syn::parenthesized!(effect_content in input);

            if effect_content.peek(Token![*]) {
                // NEW: (*) for dynamic
                effect_content.parse::<Token![*]>()?;
                StackEffectDef::Dynamic
            } else {
                let consumes: LitInt = effect_content.parse()?;
                effect_content.parse::<Token![->]>()?;
                let produces: LitInt = effect_content.parse()?;
                StackEffectDef::Fixed {
                    consumes: consumes.base10_parse()?,
                    produces: produces.base10_parse()?,
                }
            }
        };

        // Parse documentation: either a simple string or a full doc block
        let doc = if input.peek(LitStr) {
            let brief: LitStr = input.parse()?;
            DocDef {
                brief: brief.value(),
                ..Default::default()
            }
        } else if input.peek(syn::token::Bracket) {
            parse_full_doc(input)?
        } else {
            DocDef::default()
        };

        // Parse body: { ... }
        let body_content;
        braced!(body_content in input);
        let body: TokenStream = body_content.parse()?;

        Ok(CommandDef {
            name,
            aliases,
            internal,
            effect,
            doc,
            body,
        })
    }
}

fn parse_full_doc(input: ParseStream) -> Result<DocDef> {
    let content;
    syn::bracketed!(content in input);

    let mut doc = DocDef::default();

    while !content.is_empty() {
        if content.peek(kw::brief) {
            content.parse::<kw::brief>()?;
            content.parse::<Token![:]>()?;
            let s: LitStr = content.parse()?;
            doc.brief = s.value();
        } else if content.peek(kw::stack) {
            content.parse::<kw::stack>()?;
            content.parse::<Token![:]>()?;
            let s: LitStr = content.parse()?;
            doc.stack = Some(s.value());
        } else if content.peek(kw::example) {
            content.parse::<kw::example>()?;
            content.parse::<Token![:]>()?;
            let s: LitStr = content.parse()?;
            doc.example = Some(s.value());
        } else if content.peek(kw::see_also) {
            content.parse::<kw::see_also>()?;
            content.parse::<Token![:]>()?;
            // Parse array of strings: ["A", "B"]
            let array_content;
            syn::bracketed!(array_content in content);
            let items: Punctuated<LitStr, Token![,]> =
                Punctuated::parse_terminated(&array_content)?;
            doc.see_also = items.into_iter().map(|s| s.value()).collect();
        } else {
            return Err(content.error("expected 'brief', 'stack', 'example', or 'see_also'"));
        }

        // Optional comma between fields
        let _ = content.parse::<Token![,]>();
    }

    Ok(doc)
}

/// Parse a construct definition:
/// NEW syntax: "{": open(List);  or  "}": close;
fn parse_construct(input: ParseStream) -> Result<ConstructDef> {
    // Parse token string: "{" or "}"
    let token_lit: LitStr = input.parse()?;
    let token = token_lit.value();

    // Parse : (colon)
    input.parse::<Token![:]>()?;

    // Parse action: open(Kind) or close
    let action = if input.peek(kw::open) {
        input.parse::<kw::open>()?;
        let kind_content;
        syn::parenthesized!(kind_content in input);
        let kind: Ident = kind_content.parse()?;
        ConstructAction::Open(kind.to_string())
    } else if input.peek(kw::close) {
        input.parse::<kw::close>()?;
        ConstructAction::Close
    } else {
        return Err(input.error("expected 'open' or 'close'"));
    };

    // Parse semicolon
    input.parse::<Token![;]>()?;

    Ok(ConstructDef { token, action })
}

/// Parse an operator definition:
/// NEW syntax: Add(Symmetric REAL) -> CMD_ADD { commutative };
/// NEW syntax: Mul(Binary MATRIX, REAL -> MATRIX) -> CMD_MUL { priority: 50 };
fn parse_operator(input: ParseStream) -> Result<OperatorDef> {
    // Parse operator kind: Add, Sub, Neg, etc.
    let kind_ident: Ident = input.parse()?;
    let kind = kind_ident.to_string();

    // Parse signature in parentheses: (Symmetric REAL), (Unary REAL), (Binary LEFT, RIGHT -> RESULT)
    let sig_content;
    syn::parenthesized!(sig_content in input);
    let (signature, result_from_sig) = parse_op_signature(&sig_content)?;

    // Parse -> CMD_NAME
    input.parse::<Token![->]>()?;
    let cmd_ident: Ident = input.parse()?;
    let command = cmd_ident.to_string();

    // Parse optional modifiers block: { commutative, priority: 50 }
    let (result_from_mod, priority, commutative) = if input.peek(syn::token::Brace) {
        parse_operator_modifiers(input)?
    } else {
        (None, 100, false)
    };

    // Parse semicolon
    input.parse::<Token![;]>()?;

    // Result type can come from signature (Binary X, Y -> Z) or modifiers block
    let result_type = result_from_mod.or(result_from_sig);

    Ok(OperatorDef {
        kind,
        command,
        signature,
        result_type,
        priority,
        commutative,
    })
}

/// Parse operator signature: Symmetric REAL, Unary REAL, Binary LEFT, RIGHT -> RESULT
fn parse_op_signature(input: ParseStream) -> Result<(OpSigDef, Option<String>)> {
    let mut result_type = None;

    let sig = if input.peek(kw::Symmetric) {
        input.parse::<kw::Symmetric>()?;
        let ty: Ident = input.parse()?;
        OpSigDef::Symmetric(ty.to_string())
    } else if input.peek(kw::Unary) {
        input.parse::<kw::Unary>()?;
        let ty: Ident = input.parse()?;
        OpSigDef::Unary(ty.to_string())
    } else if input.peek(kw::Binary) {
        input.parse::<kw::Binary>()?;
        let left: Ident = input.parse()?;
        input.parse::<Token![,]>()?;
        let right: Ident = input.parse()?;

        // Check for -> ResultType
        if input.peek(Token![->]) {
            input.parse::<Token![->]>()?;
            let result: Ident = input.parse()?;
            result_type = Some(result.to_string());
        }

        OpSigDef::Binary {
            left: left.to_string(),
            right: right.to_string(),
        }
    } else {
        return Err(input.error("expected 'Symmetric', 'Unary', or 'Binary'"));
    };

    Ok((sig, result_type))
}

/// Parse operator modifiers block: { commutative, priority: 50, result: TYPE }
fn parse_operator_modifiers(input: ParseStream) -> Result<(Option<String>, u8, bool)> {
    let content;
    braced!(content in input);

    let mut result_type = None;
    let mut priority = 100u8;
    let mut commutative = false;

    while !content.is_empty() {
        if content.peek(kw::priority) {
            content.parse::<kw::priority>()?;
            content.parse::<Token![:]>()?;
            let p: LitInt = content.parse()?;
            priority = p.base10_parse()?;
        } else if content.peek(kw::commutative) {
            content.parse::<kw::commutative>()?;
            commutative = true;
        } else if content.peek(kw::result) {
            content.parse::<kw::result>()?;
            content.parse::<Token![:]>()?;
            let ty: Ident = content.parse()?;
            result_type = Some(ty.to_string());
        } else {
            return Err(content.error("expected 'priority', 'commutative', or 'result'"));
        }

        // Optional comma
        let _ = content.parse::<Token![,]>();
    }

    Ok((result_type, priority, commutative))
}

/// Parse a literal definition: name: CodecType;
fn parse_literal(input: ParseStream) -> Result<LiteralDef> {
    // Parse name: codec_path;
    let name: Ident = input.parse()?;
    input.parse::<Token![:]>()?;
    let codec: Path = input.parse()?;

    // Parse semicolon
    input.parse::<Token![;]>()?;

    Ok(LiteralDef { name, codec })
}

/// Parse a prolog definition:
/// NEW syntax: LIST: delimited("{", "}");
/// NEW syntax: REAL: format(RealCodec);
/// NEW syntax: SYMBOLIC: custom { ... };
fn parse_prolog(input: ParseStream) -> Result<PrologDef> {
    // Parse type ID name (e.g., LIST, REAL, SYMBOLIC)
    let type_id: Ident = input.parse()?;

    // Parse : (colon)
    input.parse::<Token![:]>()?;

    // Parse prolog kind
    let kind = if input.peek(kw::delimited) {
        input.parse::<kw::delimited>()?;
        let delim_content;
        syn::parenthesized!(delim_content in input);
        let open: LitStr = delim_content.parse()?;
        delim_content.parse::<Token![,]>()?;
        let close: LitStr = delim_content.parse()?;
        PrologKind::Delimited {
            open: open.value(),
            close: close.value(),
        }
    } else if input.peek(kw::format) {
        input.parse::<kw::format>()?;
        let codec_content;
        syn::parenthesized!(codec_content in input);
        let codec: Path = codec_content.parse()?;
        PrologKind::Format(codec)
    } else if input.peek(kw::format_codec) {
        // COMPAT: Keep old format_codec syntax
        input.parse::<kw::format_codec>()?;
        let codec_content;
        syn::parenthesized!(codec_content in input);
        let codec: Path = codec_content.parse()?;
        PrologKind::Format(codec)
    } else if input.peek(kw::custom) {
        input.parse::<kw::custom>()?;
        let body_content;
        braced!(body_content in input);
        let body: TokenStream = body_content.parse()?;
        PrologKind::Custom(body)
    } else {
        return Err(input.error("expected 'delimited', 'format', or 'custom'"));
    };

    // Parse semicolon
    input.parse::<Token![;]>()?;

    Ok(PrologDef { type_id, kind })
}

/// Parse a control pattern definition:
/// NEW syntax: if_then_else(IF, THEN, END) { alt: ELSE };
/// NEW syntax: start_next(START, NEXT) { step: STEP };
fn parse_control_pattern(input: ParseStream) -> Result<ControlPatternDef> {
    // Determine pattern type by keyword
    let pattern = if input.peek(kw::if_then_else) {
        input.parse::<kw::if_then_else>()?;
        ControlPattern::IfThenElse
    } else if input.peek(kw::do_until) {
        input.parse::<kw::do_until>()?;
        ControlPattern::DoUntil
    } else if input.peek(kw::while_repeat) {
        input.parse::<kw::while_repeat>()?;
        ControlPattern::WhileRepeat
    } else if input.peek(kw::start_next) {
        input.parse::<kw::start_next>()?;
        ControlPattern::StartNext
    } else if input.peek(kw::for_next) {
        input.parse::<kw::for_next>()?;
        ControlPattern::ForNext
    } else if input.peek(kw::inline_conditional) {
        input.parse::<kw::inline_conditional>()?;
        ControlPattern::InlineConditional
    } else if input.peek(kw::case_pattern) {
        input.parse::<kw::case_pattern>()?;
        ControlPattern::Case
    } else if input.peek(kw::error_handler) {
        input.parse::<kw::error_handler>()?;
        ControlPattern::ErrorHandler
    } else {
        return Err(input.error("expected control pattern type"));
    };

    // Parse positional keywords in parentheses: (IF, THEN, END)
    let args_content;
    syn::parenthesized!(args_content in input);

    let keywords: Punctuated<Ident, Token![,]> = Punctuated::parse_terminated(&args_content)?;
    let keywords: Vec<Ident> = keywords.into_iter().collect();

    // Validate arity
    if keywords.len() != pattern.arity() {
        return Err(input.error(format!(
            "{:?} requires {} keywords, got {}",
            pattern,
            pattern.arity(),
            keywords.len()
        )));
    }

    // Parse optional modifiers block: { alt: ELSE }
    let options = if input.peek(syn::token::Brace) {
        parse_control_options(input, pattern)?
    } else {
        ControlPatternOptions::default()
    };

    // Parse semicolon
    input.parse::<Token![;]>()?;

    Ok(ControlPatternDef {
        pattern,
        keywords,
        options,
    })
}

/// Parse control pattern options block: { alt: ELSE, step: STEP }
fn parse_control_options(input: ParseStream, pattern: ControlPattern) -> Result<ControlPatternOptions> {
    let content;
    braced!(content in input);

    let mut options = ControlPatternOptions::default();

    while !content.is_empty() {
        if content.peek(kw::alt) {
            if pattern != ControlPattern::IfThenElse {
                return Err(content.error("'alt' only valid for if_then_else"));
            }
            content.parse::<kw::alt>()?;
            content.parse::<Token![:]>()?;
            options.alt = Some(content.parse()?);
        } else if content.peek(kw::step) {
            if pattern != ControlPattern::StartNext {
                return Err(content.error("'step' only valid for start_next"));
            }
            content.parse::<kw::step>()?;
            content.parse::<Token![:]>()?;
            options.step = Some(content.parse()?);
        } else if content.peek(kw::no_error) {
            if pattern != ControlPattern::ErrorHandler {
                return Err(content.error("'no_error' only valid for error_handler"));
            }
            content.parse::<kw::no_error>()?;
            content.parse::<Token![:]>()?;
            options.no_error = Some(content.parse()?);
        } else {
            return Err(content.error("unexpected option"));
        }

        // Optional comma
        let _ = content.parse::<Token![,]>();
    }

    Ok(options)
}

/// Parse a coercion definition:
/// NEW syntax: BINT -> REAL -> CMD_BINT_TO_REAL { implicit, priority: 50 };
fn parse_coercion(input: ParseStream) -> Result<CoercionDef> {
    // Parse: From -> To -> Command
    let from: Ident = input.parse()?;
    input.parse::<Token![->]>()?;
    let to: Ident = input.parse()?;
    input.parse::<Token![->]>()?;
    let command: Ident = input.parse()?;

    // Parse optional modifiers block: { implicit, priority: 50 }
    let (priority, implicit) = if input.peek(syn::token::Brace) {
        parse_coercion_modifiers(input)?
    } else {
        (100, false)
    };

    // Parse semicolon
    input.parse::<Token![;]>()?;

    Ok(CoercionDef {
        from,
        to,
        command,
        priority,
        implicit,
    })
}

/// Parse coercion modifiers block: { implicit, priority: 50 }
fn parse_coercion_modifiers(input: ParseStream) -> Result<(u8, bool)> {
    let content;
    braced!(content in input);

    let mut priority = 100u8;
    let mut implicit = false;

    while !content.is_empty() {
        if content.peek(kw::priority) {
            content.parse::<kw::priority>()?;
            content.parse::<Token![:]>()?;
            let p: LitInt = content.parse()?;
            priority = p.base10_parse()?;
        } else if content.peek(kw::implicit) {
            content.parse::<kw::implicit>()?;
            implicit = true;
        } else {
            return Err(content.error("expected 'priority' or 'implicit'"));
        }

        // Optional comma
        let _ = content.parse::<Token![,]>();
    }

    Ok((priority, implicit))
}

/// Parse an operator syntax definition:
/// NEW syntax: "+" | "plus"(Add, binary) { infix: 10 };
/// NEW syntax: "^"(Pow, binary) { infix: 30, right_assoc };
/// NEW syntax: "NEG"(Neg, unary);
fn parse_operator_syntax(input: ParseStream) -> Result<OperatorSyntaxDef> {
    // Parse tokens: "+" | "plus"
    let mut tokens = Vec::new();
    loop {
        let token: LitStr = input.parse()?;
        tokens.push(token.value());
        if input.peek(Token![|]) {
            input.parse::<Token![|]>()?;
        } else {
            break;
        }
    }

    // Parse (Kind, arity) in parentheses
    let args_content;
    syn::parenthesized!(args_content in input);

    let kind: Ident = args_content.parse()?;
    args_content.parse::<Token![,]>()?;

    let arity = if args_content.peek(kw::unary) {
        args_content.parse::<kw::unary>()?;
        Arity::Unary
    } else if args_content.peek(kw::binary) {
        args_content.parse::<kw::binary>()?;
        Arity::Binary
    } else {
        return Err(args_content.error("expected 'unary' or 'binary'"));
    };

    // Parse optional modifiers block: { infix: 10, right_assoc }
    let (infix_precedence, right_assoc) = if input.peek(syn::token::Brace) {
        parse_syntax_modifiers(input)?
    } else {
        (None, false)
    };

    // Parse semicolon
    input.parse::<Token![;]>()?;

    Ok(OperatorSyntaxDef {
        tokens,
        kind: kind.to_string(),
        arity,
        infix_precedence,
        right_assoc,
    })
}

/// Parse operator syntax modifiers block: { infix: 10, right_assoc }
fn parse_syntax_modifiers(input: ParseStream) -> Result<(Option<u8>, bool)> {
    let content;
    braced!(content in input);

    let mut infix_precedence = None;
    let mut right_assoc = false;

    while !content.is_empty() {
        if content.peek(kw::infix) {
            content.parse::<kw::infix>()?;
            content.parse::<Token![:]>()?;
            let p: LitInt = content.parse()?;
            infix_precedence = Some(p.base10_parse()?);
        } else if content.peek(kw::right_assoc) {
            content.parse::<kw::right_assoc>()?;
            right_assoc = true;
        } else {
            return Err(content.error("expected 'infix' or 'right_assoc'"));
        }

        // Optional comma
        let _ = content.parse::<Token![,]>();
    }

    Ok((infix_precedence, right_assoc))
}
