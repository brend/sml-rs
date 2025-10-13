use syntax::{Name, Pat};

fn parse_pat_hook(src: &str) -> Result<crate::Pat, String> {
    match parser::parse_pat(src) {
        Ok(p) => Ok(p),
        Err(e) => Err(format!("Parse error: {}", e.message)),
    }
}

#[test]
fn test_as_pattern() {
    let p = parse_pat_hook("x as (y, z)").unwrap();
    println!("Parsed pattern: {:?}", p);
    assert!(matches!(p, Pat::As { name: Name { text }, pat: _, .. } if text == "x"));

    // Test with different patterns
    let p = parse_pat_hook("a as _").unwrap();
    assert!(matches!(p, Pat::As { name: Name { text }, pat: _, .. } if text == "a"));

    let p = parse_pat_hook("foo as 42").unwrap();
    assert!(matches!(p, Pat::As { name: Name { text }, pat: _, .. } if text == "foo"));

    // Test nested as patterns (right associative: x as y as z = x as (y as z))
    let p = parse_pat_hook("x as y as z").unwrap();
    assert!(matches!(p, Pat::As { name: Name { text }, pat: _, .. } if text == "x"));
}

#[test]
fn test_as_pattern_errors() {
    // As-pattern requires variable on left side
    let result = parse_pat_hook("42 as x");
    assert!(result.is_err());

    let result = parse_pat_hook("(a, b) as x");
    assert!(result.is_err());
}
