#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub parser); // synthesized by LALRPOP

#[test]
fn test_int() {
    assert_eq!(parser::IntParser::new().parse("100"), Ok(100));
    assert_eq!(parser::IntParser::new().parse("-22"), Ok(-22));
    assert!(parser::IntParser::new().parse("(22)").is_err());
}

#[test]
fn test_simpleexpr() {
    assert!(parser::ExprParser::new().parse("25").is_ok());
    assert!(parser::ExprParser::new().parse("25.0").is_ok());
    assert!(parser::ExprParser::new().parse("-12.4").is_ok());
    assert!(parser::ExprParser::new().parse("false").is_ok());
}
fn main() {}
