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

#[test]
fn test_calc() {
    assert!(parser::ExprParser::new().parse("25 * 20").is_ok());
    assert!(parser::ExprParser::new().parse("25 + 0 * 12").is_ok());
    assert!(parser::ExprParser::new().parse("-12.4 +. 20 *. 2").is_ok());
    assert!(parser::ExprParser::new().parse("1.0/.2.0").is_ok());
}

#[test]
fn test_cond() {
    assert!(parser::ExprParser::new().parse("25 < 20 * 20").is_ok());
    assert!(parser::ExprParser::new().parse("25.2 <> 2.3").is_ok());
}

#[test]
fn test_if() {
    assert!(
        parser::ExprParser::new()
            .parse("if true then 2.0 else if 2.0 <> 3.0 then 2.0 +. 3.0 /. 2.3 else 4.0 ")
            .is_ok()
    );
}
#[test]
fn test_let() {
    assert!(
        parser::ExprParser::new()
            .parse("let x = 1 in let y = 2 in x + y")
            .is_ok()
    );
    assert!(
        parser::ExprParser::new()
            .parse("let x = 1 in let y = 2 in ")
            .is_err()
    );
}
#[test]
fn test_fun() {
    assert!(
        parser::ExprParser::new()
            .parse("let rec add x y = x + y in 0 ")
            .is_ok()
    );
    assert!(
        parser::ExprParser::new()
            .parse("let rec id x = x in 0 ")
            .is_ok()
    );
}

#[test]
fn test_tuple() {
    assert!(
        parser::ExprParser::new()
            .parse("let rec make_tuple x y = (x,y) in 0 ")
            .is_ok()
    );
    assert!(
        parser::ExprParser::new()
            .parse("let p = (x,y) in 3 ")
            .is_ok()
    );
}

#[test]
fn test_let_tuple() {
    assert!(
        parser::ExprParser::new()
            .parse("let (x,y) = (1,2) in x ")
            .is_ok()
    );
    assert!(
        parser::ExprParser::new()
            .parse("let (x)  = (1) in  3 ")
            .is_err()
    );
}

fn main() {}
