//! Parser for library interface declarations.
//!
//! Parses the unified syntax: `inputs -> PATTERN -> outputs`

use super::ast::*;

/// Parse error with location.
#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}: {}", self.line, self.message)
    }
}

impl std::error::Error for ParseError {}

/// Parse a complete library interface.
pub fn parse(input: &str) -> Result<Library, ParseError> {
    let mut name = String::new();
    let mut id = 0u16;
    let mut declarations = Vec::new();

    for (line_num, line) in input.lines().enumerate() {
        let line = line.trim();

        // Skip empty lines and comments
        if line.is_empty() || line.starts_with("--") || line.starts_with("//") {
            continue;
        }

        // Library declaration
        if line.starts_with("library ") {
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() != 3 {
                return Err(ParseError {
                    message: "expected: library Name ID".into(),
                    line: line_num + 1,
                });
            }
            name = parts[1].to_string();
            id = parts[2].parse().map_err(|_| ParseError {
                message: format!("invalid library ID: {}", parts[2]),
                line: line_num + 1,
            })?;
            continue;
        }

        // Declaration: inputs -> PATTERN -> outputs
        if line.contains("->") {
            let decl = parse_declaration(line).map_err(|msg| ParseError {
                message: msg,
                line: line_num + 1,
            })?;
            declarations.push(decl);
            continue;
        }

        return Err(ParseError {
            message: format!("unexpected: {}", line),
            line: line_num + 1,
        });
    }

    if name.is_empty() {
        return Err(ParseError {
            message: "missing library declaration".into(),
            line: 1,
        });
    }

    Ok(Library {
        name,
        id,
        declarations,
    })
}

/// Parse a single declaration line.
fn parse_declaration(line: &str) -> Result<Declaration, String> {
    // Check for explicit ID prefix: "N: ..."
    let (id, rest) = parse_id_prefix(line)?;

    // Split on -> but need to handle multiple arrows
    // Format: inputs -> PATTERN -> outputs
    let parts: Vec<&str> = rest.split("->").collect();

    if parts.len() < 3 {
        return Err("expected: id: inputs -> pattern -> outputs".into());
    }

    // With 3+ parts, first is inputs, last is outputs, middle is pattern
    let inputs_str = parts[0].trim();
    let outputs_str = parts.last().unwrap().trim();

    // Middle parts (could have -> in pattern for arrow operators)
    let pattern_str = if parts.len() == 3 {
        parts[1].trim().to_string()
    } else {
        parts[1..parts.len() - 1].join("->")
    };

    let inputs = parse_types(inputs_str)?;
    let pattern = parse_pattern(&pattern_str)?;
    let outputs = parse_types(outputs_str)?;

    Ok(Declaration {
        id,
        inputs,
        pattern,
        outputs,
    })
}

/// Parse the ID prefix from a declaration line.
/// Format: "N: rest" where N is a u16.
fn parse_id_prefix(line: &str) -> Result<(u16, &str), String> {
    // Find the first colon
    let Some(colon_pos) = line.find(':') else {
        return Err("expected id prefix (e.g., '0: -> DUP -> ...')".into());
    };

    let id_str = line[..colon_pos].trim();
    let rest = line[colon_pos + 1..].trim();

    let id = id_str
        .parse::<u16>()
        .map_err(|_| format!("invalid declaration ID: '{}'", id_str))?;

    Ok((id, rest))
}

/// Parse a space-separated list of types.
fn parse_types(s: &str) -> Result<Vec<Type>, String> {
    if s.is_empty() {
        return Ok(Vec::new());
    }

    let mut types = Vec::new();
    let tokens: Vec<&str> = s.split_whitespace().collect();
    let mut i = 0;

    while i < tokens.len() {
        let token = tokens[i];

        // Dynamic
        if token == "..." {
            types.push(Type::Dynamic);
            i += 1;
            continue;
        }

        // Numeric a b
        if token == "Numeric" {
            if i + 2 >= tokens.len() {
                return Err("Numeric requires two type variables".into());
            }
            let a = parse_var(tokens[i + 1])?;
            let b = parse_var(tokens[i + 2])?;
            types.push(Type::Numeric(a, b));
            i += 3;
            continue;
        }

        // Check for union: a | b
        if i + 2 < tokens.len() && tokens[i + 1] == "|" {
            let a = parse_single_type(token)?;
            let b = parse_single_type(tokens[i + 2])?;
            types.push(Type::Union(Box::new(a), Box::new(b)));
            i += 3;
            continue;
        }

        // Single type
        types.push(parse_single_type(token)?);
        i += 1;
    }

    Ok(types)
}

/// Parse a single type token.
fn parse_single_type(s: &str) -> Result<Type, String> {
    // Concrete type
    if let Some(ct) = ConcreteType::parse(s) {
        return Ok(Type::Concrete(ct));
    }

    // Type variable (single lowercase letter)
    if s.len() == 1 {
        let c = s.chars().next().unwrap();
        if c.is_ascii_lowercase() {
            return Ok(Type::Var(c));
        }
    }

    Err(format!("unknown type: {}", s))
}

/// Parse a type variable.
fn parse_var(s: &str) -> Result<char, String> {
    if s.len() == 1 {
        let c = s.chars().next().unwrap();
        if c.is_ascii_lowercase() {
            return Ok(c);
        }
    }
    Err(format!("expected type variable, got: {}", s))
}

/// Parse the pattern part.
fn parse_pattern(s: &str) -> Result<Pattern, String> {
    let tokens: Vec<&str> = s.split_whitespace().collect();
    if tokens.is_empty() {
        return Err("empty pattern".into());
    }

    let mut names = Vec::new();
    let mut slots = Vec::new();
    let mut in_names = true;
    let mut i = 0;

    while i < tokens.len() {
        let token = tokens[i];

        // Check for group: ( ... )* for repeat, ( A | B ) for alternation
        if token == "(" {
            in_names = false;
            let start = i + 1;
            let mut depth = 1;
            let mut end_idx = i + 1;
            let mut is_repeat = false;

            // Find the matching close
            while end_idx < tokens.len() {
                if tokens[end_idx] == "(" {
                    depth += 1;
                } else if tokens[end_idx] == ")*" {
                    depth -= 1;
                    if depth == 0 {
                        is_repeat = true;
                        break;
                    }
                } else if tokens[end_idx] == ")" {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                end_idx += 1;
            }

            if end_idx >= tokens.len() {
                return Err("unclosed group: expected ) or )*".into());
            }

            let inner_tokens = &tokens[start..end_idx];

            if is_repeat {
                // Repeat group: ( ... )*
                let inner_pattern = inner_tokens.join(" ");
                let inner_elements = parse_pattern_elements(&inner_pattern)?;
                slots.push(PatternElement::Repeat(inner_elements));
            } else {
                // Alternation: ( A | B )
                let inner_str = inner_tokens.join(" ");
                let alternatives: Vec<&str> = inner_str.split('|').collect();
                if alternatives.len() < 2 {
                    return Err("alternation requires at least two alternatives: ( A | B )".into());
                }
                let mut alt_elements = Vec::new();
                for alt in alternatives {
                    let elements = parse_pattern_elements(alt.trim())?;
                    alt_elements.push(elements);
                }
                slots.push(PatternElement::Alternation(alt_elements));
            }

            i = end_idx + 1;
            continue;
        }

        // Check for slot: name:Type or $name:Type
        if token.contains(':') {
            in_names = false;
            let (slot_name, type_str) = token.split_once(':').unwrap();

            let typ = ConcreteType::parse(type_str)
                .ok_or_else(|| format!("unknown slot type: {}", type_str))?;

            if let Some(name) = slot_name.strip_prefix('$') {
                slots.push(PatternElement::Binding {
                    name: name.to_string(),
                    typ,
                });
            } else {
                slots.push(PatternElement::Slot {
                    name: slot_name.to_string(),
                    typ,
                });
            }
            i += 1;
            continue;
        }

        // Check for parenthesized operator: (+), (→), etc.
        if token.starts_with('(') && token.ends_with(')') {
            if in_names {
                let inner = &token[1..token.len() - 1];
                names.push(inner.to_string());
            } else {
                // It's a keyword like (→) in the pattern
                slots.push(PatternElement::Keyword(token.to_string()));
            }
            i += 1;
            continue;
        }

        // Check for comma-separated aliases
        if token.ends_with(',') {
            let name = token.trim_end_matches(',');
            if name.starts_with('(') && name.ends_with(')') {
                names.push(name[1..name.len() - 1].to_string());
            } else {
                names.push(name.to_string());
            }
            i += 1;
            continue;
        }

        // Check for optional keyword with capture: STEP?!
        if token.ends_with("?!") {
            in_names = false;
            let kw = token.trim_end_matches("?!");
            slots.push(PatternElement::OptionalKeywordWithCapture(kw.to_string()));
            i += 1;
            continue;
        }

        // Check for required keyword with capture: STEP!
        if token.ends_with('!') {
            in_names = false;
            let kw = token.trim_end_matches('!');
            slots.push(PatternElement::KeywordWithCapture(kw.to_string()));
            i += 1;
            continue;
        }

        // Check for optional keyword: ELSE?, END?, etc.
        if token.ends_with('?') {
            in_names = false;
            let kw = token.trim_end_matches('?');
            slots.push(PatternElement::OptionalKeyword(kw.to_string()));
            i += 1;
            continue;
        }

        // Uppercase = keyword or command name
        // Lowercase = would be a slot without type (error) or part of something
        // Non-ASCII punctuation (like « ») = keyword in pattern context
        let first_char = token.chars().next().unwrap_or(' ');

        if in_names && (first_char.is_ascii_uppercase() || first_char == '(' || first_char == '$') {
            // First uppercase token is the command name
            names.push(token.to_string());
            // If there are more tokens, they're slots/keywords
            if !slots.is_empty() {
                in_names = false;
            }
        } else if first_char.is_ascii_uppercase() {
            // Keyword in pattern
            in_names = false;
            slots.push(PatternElement::Keyword(token.to_string()));
        } else if first_char.is_ascii_lowercase() {
            // Lowercase without colon - might be untyped slot, treat as keyword for now
            in_names = false;
            slots.push(PatternElement::Keyword(token.to_string()));
        } else if in_names {
            // Non-ASCII (like operators ≤, ≥) go to names if we're still collecting names
            names.push(token.to_string());
        } else {
            // Non-ASCII delimiters like « » are keywords in pattern context
            slots.push(PatternElement::Keyword(token.to_string()));
        }
        i += 1;
    }

    if names.is_empty() {
        return Err("pattern has no command name".into());
    }

    Ok(Pattern { names, slots })
}

/// Parse pattern elements (slots and keywords) without command names.
fn parse_pattern_elements(s: &str) -> Result<Vec<PatternElement>, String> {
    let tokens: Vec<&str> = s.split_whitespace().collect();
    let mut elements = Vec::new();
    let mut i = 0;

    while i < tokens.len() {
        let token = tokens[i];

        // Nested group: ( ... )* for repeat, ( A | B ) for alternation
        if token == "(" {
            let start = i + 1;
            let mut depth = 1;
            let mut end_idx = i + 1;
            let mut is_repeat = false;

            while end_idx < tokens.len() {
                if tokens[end_idx] == "(" {
                    depth += 1;
                } else if tokens[end_idx] == ")*" {
                    depth -= 1;
                    if depth == 0 {
                        is_repeat = true;
                        break;
                    }
                } else if tokens[end_idx] == ")" {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                end_idx += 1;
            }

            if end_idx >= tokens.len() {
                return Err("unclosed group: expected ) or )*".into());
            }

            let inner_tokens = &tokens[start..end_idx];

            if is_repeat {
                let inner_pattern = inner_tokens.join(" ");
                let inner_elements = parse_pattern_elements(&inner_pattern)?;
                elements.push(PatternElement::Repeat(inner_elements));
            } else {
                let inner_str = inner_tokens.join(" ");
                let alternatives: Vec<&str> = inner_str.split('|').collect();
                if alternatives.len() < 2 {
                    return Err("alternation requires at least two alternatives: ( A | B )".into());
                }
                let mut alt_elements = Vec::new();
                for alt in alternatives {
                    let elems = parse_pattern_elements(alt.trim())?;
                    alt_elements.push(elems);
                }
                elements.push(PatternElement::Alternation(alt_elements));
            }

            i = end_idx + 1;
            continue;
        }

        // Slot: name:Type or $name:Type
        if token.contains(':') {
            let (slot_name, type_str) = token.split_once(':').unwrap();
            let typ = ConcreteType::parse(type_str)
                .ok_or_else(|| format!("unknown slot type: {}", type_str))?;

            if let Some(name) = slot_name.strip_prefix('$') {
                elements.push(PatternElement::Binding {
                    name: name.to_string(),
                    typ,
                });
            } else {
                elements.push(PatternElement::Slot {
                    name: slot_name.to_string(),
                    typ,
                });
            }
            i += 1;
            continue;
        }

        // Optional keyword with capture: STEP?!
        if token.ends_with("?!") {
            let kw = token.trim_end_matches("?!");
            elements.push(PatternElement::OptionalKeywordWithCapture(kw.to_string()));
            i += 1;
            continue;
        }

        // Required keyword with capture: STEP!
        if token.ends_with('!') {
            let kw = token.trim_end_matches('!');
            elements.push(PatternElement::KeywordWithCapture(kw.to_string()));
            i += 1;
            continue;
        }

        // Optional keyword: ELSE?, END?, etc.
        if token.ends_with('?') {
            let kw = token.trim_end_matches('?');
            elements.push(PatternElement::OptionalKeyword(kw.to_string()));
            i += 1;
            continue;
        }

        // Keyword (uppercase)
        elements.push(PatternElement::Keyword(token.to_string()));
        i += 1;
    }

    Ok(elements)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_command() {
        let decl = parse_declaration("0: a -> DUP -> a a").unwrap();
        assert_eq!(decl.id, 0);
        assert_eq!(decl.inputs.len(), 1);
        assert_eq!(decl.outputs.len(), 2);
        assert_eq!(decl.pattern.names, vec!["DUP"]);
        assert!(decl.pattern.slots.is_empty());
    }

    #[test]
    fn test_no_inputs() {
        let decl = parse_declaration("1: -> DEPTH -> Int").unwrap();
        assert_eq!(decl.id, 1);
        assert!(decl.inputs.is_empty());
        assert_eq!(decl.outputs.len(), 1);
        assert_eq!(decl.pattern.names, vec!["DEPTH"]);
    }

    #[test]
    fn test_no_outputs() {
        let decl = parse_declaration("2: a -> DROP ->").unwrap();
        assert_eq!(decl.id, 2);
        assert_eq!(decl.inputs.len(), 1);
        assert!(decl.outputs.is_empty());
        assert_eq!(decl.pattern.names, vec!["DROP"]);
    }

    #[test]
    fn test_operator_alias() {
        let decl = parse_declaration("3: a b -> (+), ADD -> Numeric a b").unwrap();
        assert_eq!(decl.id, 3);
        assert_eq!(decl.pattern.names, vec!["+", "ADD"]);
        assert_eq!(decl.outputs.len(), 1);
        assert!(matches!(decl.outputs[0], Type::Numeric('a', 'b')));
    }

    #[test]
    fn test_dynamic() {
        let decl = parse_declaration("4: Int -> ROLL -> ...").unwrap();
        assert_eq!(decl.id, 4);
        assert_eq!(decl.outputs.len(), 1);
        assert!(matches!(decl.outputs[0], Type::Dynamic));
    }

    #[test]
    fn test_union() {
        let decl = parse_declaration("5: a b Int -> IFTE -> a | b").unwrap();
        assert_eq!(decl.id, 5);
        assert_eq!(decl.outputs.len(), 1);
        assert!(matches!(decl.outputs[0], Type::Union(_, _)));
    }

    #[test]
    fn test_syntax_if() {
        let decl = parse_declaration("10: -> IF cond:Int THEN body:Prog END ->").unwrap();
        assert_eq!(decl.id, 10);
        assert!(decl.inputs.is_empty());
        assert!(decl.outputs.is_empty());
        assert_eq!(decl.pattern.names, vec!["IF"]);
        assert_eq!(decl.pattern.slots.len(), 4);
    }

    #[test]
    fn test_syntax_for() {
        let decl =
            parse_declaration("20: Int Int -> FOR $name:Sym body:Prog NEXT ->").unwrap();
        assert_eq!(decl.id, 20);
        assert_eq!(decl.inputs.len(), 2);
        assert!(decl.outputs.is_empty());
        assert_eq!(decl.pattern.names, vec!["FOR"]);
        assert_eq!(decl.pattern.slots.len(), 3);

        // Check binding
        assert!(matches!(
            &decl.pattern.slots[0],
            PatternElement::Binding { name, .. } if name == "name"
        ));
    }

    #[test]
    fn test_library() {
        let input = r#"
library Stack 72

-- Basic operations
0: a -> DUP -> a a
1: a -> DROP ->
2: a b -> SWAP -> b a
"#;
        let lib = parse(input).unwrap();
        assert_eq!(lib.name, "Stack");
        assert_eq!(lib.id, 72);
        assert_eq!(lib.declarations.len(), 3);
    }

    #[test]
    fn test_arrow_operator() {
        let decl = parse_declaration("0: Str -> (→UTF8), TOUTF8 -> List").unwrap();
        assert_eq!(decl.id, 0);
        assert_eq!(decl.pattern.names, vec!["→UTF8", "TOUTF8"]);
    }
}
