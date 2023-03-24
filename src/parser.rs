use peg::parser;
use crate::Span;

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    While(Box<Expression>, Box<Expression>, Span),
    Bool(bool, Span),
    Integer(i64, Span),
    Float(f64, Span),
    String(String, Span),
    Unit(Span),
    Pow(Box<Expression>, Box<Expression>, Span),
    Sum(Box<Expression>, Box<Expression>, Span),
    Sub(Box<Expression>, Box<Expression>, Span),
    Div(Box<Expression>, Box<Expression>, Span),
    Mod(Box<Expression>, Box<Expression>, Span),
    Mul(Box<Expression>, Box<Expression>, Span),
    Identifier(String, Span),
    Assign(Box<Expression>, Box<Expression>, Span),
    Block(Vec<Expression>, Span),
    Gt(Box<Expression>, Box<Expression>, Span),
    Lt(Box<Expression>, Box<Expression>, Span),
    Gte(Box<Expression>, Box<Expression>, Span),
    Lte(Box<Expression>, Box<Expression>, Span),
    Eq(Box<Expression>, Box<Expression>, Span),
    Neq(Box<Expression>, Box<Expression>, Span),
    And(Box<Expression>, Box<Expression>, Span),
    Or(Box<Expression>, Box<Expression>, Span),
    Incr(Box<Expression>, Span),
    Decr(Box<Expression>, Span),
    AddAssign(Box<Expression>, Box<Expression>, Span),
    SubAssign(Box<Expression>, Box<Expression>, Span),
    Array(Vec<Expression>, Span),
    If(
        Box<Expression>,
        Box<Expression>,
        Option<Box<Expression>>,
        Span,
    ),
}

impl Expression {
    pub fn span(&self) -> &Span {
        use Expression::*;
        match self {
            Array(_, span) => span,
            AddAssign(_, _, span) => span,
            SubAssign(_, _, span) => span,
            Decr(_, span) => span,
            Incr(_, span) => span,
            While(_, _, span) => span,
            And(_, _, span) => span,
            Or(_, _, span) => span,
            Lte(_, _, span) => span,
            Gte(_, _, span) => span,
            Lt(_, _, span) => span,
            Neq(_, _, span) => span,
            Gt(_, _, span) => span,
            Eq(_, _, span) => span,
            Float(_, span) => span,
            Unit(span) => span,
            String(_, span) => span,
            Identifier(_, span) => span,
            Assign(_, _, span) => span,
            Pow(_, _, span) => span,
            Integer(_, span) => span,
            Sum(_, _, span) => span,
            Sub(_, _, span) => span,
            Div(_, _, span) => span,
            Mod(_, _, span) => span,
            Mul(_, _, span) => span,
            Block(_, span) => span,
            Bool(_, span) => span,
            If(_, _, _, span) => span,
        }
    }
}

parser! {
pub grammar lang() for str {
    rule _ = [' ' | '\n' | '\t']*
    pub rule parser() -> Expression
        = _ v:statements() _ {
            v
        }
    rule array() -> Expression
        = _ "[" _ v:(atom() ** ",") _ "]" p:position!() {
            let span = Span::new(v[0].span().start(), p);
            Expression::Array(v, span)
        }
    rule statements() -> Expression
        = _ v:(statement())+ _ {
            let span = Span::new(v[0].span().start(), v[v.len()-1].span().end());
            Expression::Block(v, span)
        }

    rule increment() -> Expression
        = _ v:identifier() _ "++" p:position!()  {
            let span = Span::new(v.span().start(), p);
            Expression::Incr(Box::new(v), span)
        }

    rule add_assign() -> Expression
        = _ v:identifier() _ "+=" _ rhs:expression() {
            let span = Span::new(v.span().start(), rhs.span().end());
            Expression::AddAssign(Box::new(v), Box::new(rhs), span)
        }
    rule sub_assign() -> Expression
        = _ v:identifier() _ "-=" _ rhs:expression() {
            let span = Span::new(v.span().start(), rhs.span().end());
            Expression::SubAssign(Box::new(v), Box::new(rhs), span)
        }
    rule decrement() -> Expression
        = _ v:identifier() _ "--" p:position!() {
            let span = Span::new(v.span().start(), p);
            Expression::Decr(Box::new(v), span)
        }

    rule postfix() -> Expression
        = v:increment()
        / v:decrement()
        / v:identifier()

    pub rule statement() -> Expression
        = x:expression() _ ";"  {
            x
        }/ x:assign()
        / if_else()
        / x:if_statement()
        / x:while_statement()



    rule else_statement() -> Expression
        = _ "else:" _ v:statements() _ "---" _ {
            v
        }

    rule if_statement() -> Expression
        = _ "if" _ cond:expression() _ ":" _ body:statements() _ "---" {
            let span = Span::new(cond.span().start(), body.span().end());
                Expression::If(Box::new(cond), Box::new(body), None, span)
        }
    rule if_else() -> Expression = if_body:if_statement() else_body:else_statement(){
            let span = Span::new(if_body.span().start(), else_body.span().end());
            let (head, tail) = if let Expression::If(head, tail, _, _) = if_body { (head, tail) } else { unreachable!() };
            Expression::If(
                Box::new(*head),
                Box::new(*tail),
                Some(Box::new(else_body)),
                span,
            )
        }
    pub rule while_statement() -> Expression
        = _ "while" _ cond:expression() _ ":" _ body:statements() _ "---" {
            let span = Span::new(cond.span().start(), body.span().end());
            Expression::While(Box::new(cond), Box::new(body), span)
        }
    pub rule comparison() -> Expression
        = lhs:atom() _ "==" _ rhs: atom() {
            let span = Span::new(lhs.span().start(), rhs.span().end());
            Expression::Eq(Box::new(lhs), Box::new(rhs), span)
        }/
        lhs:atom() _ ">=" _ rhs: atom() {
            let span = Span::new(lhs.span().start(), rhs.span().end());
            Expression::Gte(Box::new(lhs), Box::new(rhs), span)
        }/
        lhs:atom() _ "<=" _ rhs: atom() {
            let span = Span::new(lhs.span().start(), rhs.span().end());
            Expression::Lte(Box::new(lhs), Box::new(rhs), span)
        }/
        lhs:atom() _ ">" _ rhs: atom() {
            let span = Span::new(lhs.span().start(), rhs.span().end());
            Expression::Gt(Box::new(lhs), Box::new(rhs), span)
        }/
        lhs:atom() _ "<" _ rhs: atom() {
            let span = Span::new(lhs.span().start(), rhs.span().end());
            Expression::Lt(Box::new(lhs), Box::new(rhs), span)
        }/
        lhs:atom() _ "!=" _ rhs: atom() {
            let span = Span::new(lhs.span().start(), rhs.span().end());
            Expression::Neq(Box::new(lhs), Box::new(rhs), span)
        }


    pub rule expression() -> Expression
        = cmp()
        / sum()
        / postfix()

    pub rule cmp() -> Expression
        = _ lhs:atom() _ "&&" _ rhs:atom() {
            let span = Span::new(lhs.span().start(), rhs.span().end());
            Expression::And(Box::new(lhs), Box::new(rhs), span)
        }/
        _ lhs:atom() _ "||" _ rhs:atom() {
            let span = Span::new(lhs.span().start(), rhs.span().end());
            Expression::Or(Box::new(lhs), Box::new(rhs), span)
        }/
        comparison()


    rule assign() -> Expression
        = add_assign()
        / sub_assign()
        / _ l:identifier() _ "=" _ r:expression() _ ";" {
            let span = Span::new(l.span().start(), r.span().end());
            Expression::Assign(Box::new(l), Box::new(r), span)
        }

    rule identifier() -> Expression
        = _ n:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9']*) end:position!() {
            let start = end - n.len();
            Expression::Identifier(n.to_string(), Span::new(start, end))
        }


    rule sum() -> Expression
        = l:pow() _ "+" _ r:pow() {
            let span = Span::new(l.span().start(), r.span().end());
            Expression::Sum(Box::new(l), Box::new(r), span)
        }
        / pow()
    rule pow() -> Expression
        = l:product() _ "^" _ r:product() {
            let span = Span::new(l.span().start(), r.span().end());
            Expression::Pow(Box::new(l), Box::new(r), span)
        }
        / product()

    rule product() -> Expression
        = _ l:div() _ "*" _ r:div() {
            let span = Span::new(l.span().start(), r.span().end());
            Expression::Mul(Box::new(l), Box::new(r), span)
        }
        / div()

    rule div() -> Expression
        = _ l:sub() _ "/" _ r:sub() {
            let span = Span::new(l.span().start(), r.span().end());
            Expression::Div(Box::new(l), Box::new(r), span)
        }
        / sub()


    rule sub() -> Expression
        = _ l:mod() _ "-" _ r:mod() {
            let span = Span::new(l.span().start(), r.span().end());
            Expression::Sub(Box::new(l), Box::new(r),span)
        }
        / mod()

    rule mod() -> Expression
        = _ l:atom() _ "%" _ r:atom() {
            let span = Span::new(l.span().start(), r.span().end());
            Expression::Mod(Box::new(l), Box::new(r), span)
        }
        / atom()

    rule bool() -> Expression
        = "true" end:position!() {
            let start = end - 4;
            Expression::Bool(true, Span::new(start, end))
        }
        / "false" end:position!() {
            let start = end - 5;
            Expression::Bool(false, Span::new(start, end))
        }

    pub rule atom() -> Expression
        = float()
         / integer()
         / unit()
         / string()
         / postfix()
         / bool()
         / increment()
         / decrement()
         / array()
         / "(" _ v:expression() _ ")" { v }


    rule string() -> Expression
        = "\"" s:$((!['"'] [_])*) "\"" end:position!() {
            let start = end - s.len() - 2;
            Expression::String(s.to_string(), Span::new(start, end))
        }
    rule unit() -> Expression
        = "()" end:position!() {
            let start = end - 2;
            Expression::Unit(Span::new(start, end))
        }

    rule float() -> Expression
        = n:$(['0'..='9']+) "." m:$(['0'..='9']+) end:position!() {
            let start = end - n.len() - m.len() - 1;
            Expression::Float(format!("{}.{}", n, m).parse().unwrap(), Span::new(start, end))
        }

    rule integer() -> Expression
        = n:$(['0'..='9']+) end: position!() {
            let start = end - n.len();
            Expression::Integer(n.parse().unwrap(), Span::new(start, end))
        }
}}

#[cfg(test)]
mod test {
    use super::Expression::*;
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test01() {
        assert_eq!(
            lang::expression("1+1"),
            Ok(Sum(
                Box::new(Integer(1, Span::new(0, 1))),
                Box::new(Integer(1, Span::new(2, 3))),
                Span::new(0, 3)
            ))
        );
    }

    #[test]
    fn test02() {
        assert_eq!(
            lang::expression("5*5"),
            Ok(Mul(
                Box::new(Integer(5, Span::new(0, 1))),
                Box::new(Integer(5, Span::new(2, 3))),
                Span::new(0, 3)
            ))
        );
    }
    #[test]
    fn test03() {
        assert_eq!(
            lang::expression("2+3*4"),
            Ok(Sum(
                Box::new(Integer(2, Span::new(0, 1))),
                Box::new(Mul(
                    Box::new(Integer(3, Span::new(2, 3))),
                    Box::new(Integer(4, Span::new(4, 5))),
                    Span::new(2, 5)
                )),
                Span::new(0, 5)
            ))
        );
    }

    #[test]
    fn test04() {
        assert_eq!(
            lang::expression("(2+3) * 4"),
            Ok(Mul(
                Box::new(Sum(
                    Box::new(Integer(2, Span::new(1, 2))),
                    Box::new(Integer(3, Span::new(3, 4))),
                    Span::new(1, 4)
                )),
                Box::new(Integer(4, Span::new(8, 9))),
                Span::new(1, 9)
            ))
        );
    }
    #[test]
    fn test05() {
        assert!(lang::expression("(22+)+1").is_err());
    }
    #[test]
    fn test06() {
        assert!(lang::expression("1++1").is_err());
    }
    #[test]
    fn test07() {
        assert!(lang::expression("3)+1").is_err());
    }
    #[test]
    fn test08() {
        assert_eq!(
            lang::expression("1/2"),
            Ok(Div(
                Box::new(Integer(1, Span::new(0, 1))),
                Box::new(Integer(2, Span::new(2, 3))),
                Span::new(0, 3)
            ))
        );
    }
    #[test]
    fn test09() {
        assert_eq!(
            lang::expression("1%2"),
            Ok(Mod(
                Box::new(Integer(1, Span::new(0, 1))),
                Box::new(Integer(2, Span::new(2, 3))),
                Span::new(0, 3)
            ))
        );
    }
    #[test]
    fn test10() {
        assert_eq!(
            lang::expression("1-2"),
            Ok(Sub(
                Box::new(Integer(1, Span::new(0, 1))),
                Box::new(Integer(2, Span::new(2, 3))),
                Span::new(0, 3)
            ))
        );
    }
    #[test]
    fn test11() {
        assert_eq!(
            lang::expression("1+2-3"),
            Ok(Sum(
                Box::new(Integer(1, Span::new(0, 1))),
                Box::new(Sub(
                    Box::new(Integer(2, Span::new(2, 3))),
                    Box::new(Integer(3, Span::new(4, 5))),
                    Span::new(2, 5)
                )),
                Span::new(0, 5)
            ))
        );
    }
    #[test]
    fn test12() {
        assert_eq!(
            lang::expression("1+2^3"),
            Ok(Sum(
                Box::new(Integer(1, Span::new(0, 1))),
                Box::new(Pow(
                    Box::new(Integer(2, Span::new(2, 3))),
                    Box::new(Integer(3, Span::new(4, 5))),
                    Span::new(2, 5)
                )),
                Span::new(0, 5)
            ))
        );
    }
    #[test]
    fn test13() {
        assert_eq!(
            lang::statement("1+2;"),
            Ok(Sum(
                Box::new(Integer(1, Span::new(0, 1))),
                Box::new(Integer(2, Span::new(2, 3))),
                Span::new(0, 3)
            ))
        );
    }
    #[test]
    fn test14() {
        assert_eq!(
            lang::statement("a=10;"),
            Ok(Assign(
                Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                Box::new(Integer(10, Span::new(2, 4))),
                Span::new(0, 4)
            ))
        );
    }
    #[test]
    fn test15() {
        assert_eq!(
            lang::parser("a=10;b=20;"),
            Ok(Block(
                vec![
                    Assign(
                        Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                        Box::new(Integer(10, Span::new(2, 4))),
                        Span::new(0, 4)
                    ),
                    Assign(
                        Box::new(Identifier("b".to_string(), Span::new(5, 6))),
                        Box::new(Integer(20, Span::new(7, 9))),
                        Span::new(5, 9)
                    )
                ],
                Span::new(0, 9)
            ))
        );
    }
    #[test]
    fn test16() {
        assert_eq!(
            lang::parser("a=10/2;b=20/2;"),
            Ok(Block(
                vec![
                    Assign(
                        Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                        Box::new(Div(
                            Box::new(Integer(10, Span::new(2, 4))),
                            Box::new(Integer(2, Span::new(5, 6))),
                            Span::new(2, 6)
                        )),
                        Span::new(0, 6)
                    ),
                    Assign(
                        Box::new(Identifier("b".to_string(), Span::new(7, 8))),
                        Box::new(Div(
                            Box::new(Integer(20, Span::new(9, 11))),
                            Box::new(Integer(2, Span::new(12, 13))),
                            Span::new(9, 13)
                        )),
                        Span::new(7, 13)
                    )
                ],
                Span::new(0, 13)
            ))
        );
    }
    #[test]
    fn test17() {
        assert_eq!(
            lang::parser("a=b;"),
            Ok(Block(
                vec![Assign(
                    Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                    Box::new(Identifier("b".to_string(), Span::new(2, 3))),
                    Span::new(0, 3)
                )],
                Span::new(0, 3)
            ))
        );
    }

    #[test]
    fn test18() {
        assert_eq!(
            lang::parser("a=();"),
            Ok(Block(
                vec![Assign(
                    Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                    Box::new(Unit(Span::new(2, 4))),
                    Span::new(0, 4)
                )],
                Span::new(0, 4)
            ))
        );
    }

    #[test]
    fn test19() {
        assert_eq!(
            lang::parser("a=\"Hello, world!\";"),
            Ok(Block(
                vec![Assign(
                    Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                    Box::new(String("Hello, world!".to_string(), Span::new(2, 17))),
                    Span::new(0, 17)
                )],
                Span::new(0, 17)
            ))
        );
    }
    #[test]
    fn test20() {
        assert_eq!(
            lang::parser("1.2;"),
            Ok(Block(vec![Float(1.2, Span::new(0, 3))], Span::new(0, 3)))
        );
    }
    #[test]
    fn test21() {
        assert_eq!(
            lang::parser("a=1.2;"),
            Ok(Block(
                vec![Assign(
                    Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                    Box::new(Float(1.2, Span::new(2, 5))),
                    Span::new(0, 5)
                )],
                Span::new(0, 5)
            ))
        );
    }
    #[test]
    fn test22() {
        assert_eq!(
            lang::parser("a=1.2+3.4;"),
            Ok(Block(
                vec![Assign(
                    Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                    Box::new(Sum(
                        Box::new(Float(1.2, Span::new(2, 5))),
                        Box::new(Float(3.4, Span::new(6, 9))),
                        Span::new(2, 9)
                    )),
                    Span::new(0, 9)
                )],
                Span::new(0, 9)
            ))
        );
    }
    #[test]
    fn test23() {
        assert_eq!(
            lang::parser("a = a / 2;"),
            Ok(Block(
                vec![Assign(
                    Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                    Box::new(Div(
                        Box::new(Identifier("a".to_string(), Span::new(4, 5))),
                        Box::new(Integer(2, Span::new(8, 9))),
                        Span::new(4, 9)
                    )),
                    Span::new(0, 9)
                )],
                Span::new(0, 9)
            ))
        );
    }
    #[test]
    fn test24() {
        assert_eq!(
            lang::expression("a < b"),
            Ok(Lt(
                Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                Box::new(Identifier("b".to_string(), Span::new(4, 5))),
                Span::new(0, 5)
            ))
        );
    }
    #[test]
    fn test25() {
        assert_eq!(
            lang::expression("a > b"),
            Ok(Gt(
                Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                Box::new(Identifier("b".to_string(), Span::new(4, 5))),
                Span::new(0, 5)
            ))
        );
    }
    #[test]
    fn test26() {
        assert_eq!(
            lang::expression("a <= b"),
            Ok(Lte(
                Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                Box::new(Identifier("b".to_string(), Span::new(5, 6))),
                Span::new(0, 6)
            ))
        );
    }
    #[test]
    fn test27() {
        assert_eq!(
            lang::expression("a >= b"),
            Ok(Gte(
                Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                Box::new(Identifier("b".to_string(), Span::new(5, 6))),
                Span::new(0, 6)
            ))
        );
    }
    #[test]
    fn test28() {
        assert_eq!(
            lang::expression("a == b"),
            Ok(Eq(
                Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                Box::new(Identifier("b".to_string(), Span::new(5, 6))),
                Span::new(0, 6)
            ))
        );
    }
    #[test]
    fn test29() {
        assert_eq!(
            lang::expression("a != b"),
            Ok(Neq(
                Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                Box::new(Identifier("b".to_string(), Span::new(5, 6))),
                Span::new(0, 6)
            ))
        );
    }
    #[test]
    fn test30() {
        assert_eq!(
            lang::expression("a && b"),
            Ok(And(
                Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                Box::new(Identifier("b".to_string(), Span::new(5, 6))),
                Span::new(0, 6)
            ))
        );
    }
    #[test]
    fn test31() {
        assert_eq!(
            lang::expression("a || b"),
            Ok(Or(
                Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                Box::new(Identifier("b".to_string(), Span::new(5, 6))),
                Span::new(0, 6)
            ))
        );
    }
    #[test]
    fn test32() {
        assert_eq!(
            lang::statement("if a < b :\n1;\n---"),
            Ok(If(
                Box::new(Lt(
                    Box::new(Identifier("a".to_string(), Span::new(3, 4))),
                    Box::new(Identifier("b".to_string(), Span::new(7, 8))),
                    Span::new(3, 8)
                )),
                Box::new(Block(
                    vec![Integer(1, Span::new(11, 12))],
                    Span::new(11, 12)
                )),
                None,
                Span::new(3, 12)
            ))
        );
    }
    #[test]
    fn test33() {
        assert_eq!(
            lang::statement("if a < b :\n1;\n---\nelse:\n2;\n---"),
            Ok(If(
                Box::new(Lt(
                    Box::new(Identifier("a".to_string(), Span::new(3, 4))),
                    Box::new(Identifier("b".to_string(), Span::new(7, 8))),
                    Span::new(3, 8)
                )),
                Box::new(Block(
                    vec![Integer(1, Span::new(11, 12))],
                    Span::new(11, 12)
                )),
                Some(Box::new(Block(
                    vec![Integer(2, Span::new(24, 25))],
                    Span::new(24, 25)
                ))),
                Span::new(3, 25)
            ))
        );
    }
    #[test]
    fn test34() {
        assert_eq!(
            lang::statement("while a < b :\n1;\n---"),
            Ok(While(
                Box::new(Lt(
                    Box::new(Identifier("a".to_string(), Span::new(6, 7))),
                    Box::new(Identifier("b".to_string(), Span::new(10, 11))),
                    Span::new(6, 11)
                )),
                Box::new(Block(
                    vec![Integer(1, Span::new(14, 15))],
                    Span::new(14, 15)
                )),
                Span::new(6, 15)
            ))
        );
    }
    #[test]
    fn test35() {
        assert_eq!(
            lang::expression("i++"),
            Ok(Incr(
                Box::new(Identifier("i".to_string(), Span::new(0, 1))),
                Span::new(0, 3)
            ))
        );
    }
    #[test]
    fn test36() {
        assert_eq!(
            lang::expression("i--"),
            Ok(Decr(
                Box::new(Identifier("i".to_string(), Span::new(0, 1))),
                Span::new(0, 3)
            ))
        );
    }
    #[test]
    fn test37() {
        assert_eq!(
            lang::expression("i"),
            Ok(Identifier("i".to_string(), Span::new(0, 1)))
        );
    }
    #[test]
    fn test38() {
        assert_eq!(
            lang::statement("i += 1"),
            Ok(AddAssign(
                Box::new(Identifier("i".to_string(), Span::new(0, 1))),
                Box::new(Integer(1, Span::new(5, 6))),
                Span::new(0, 6)
            ))
        );
    }
    #[test]
    fn test39() {
        assert_eq!(
            lang::statement("i -= 1"),
            Ok(SubAssign(
                Box::new(Identifier("i".to_string(), Span::new(0, 1))),
                Box::new(Integer(1, Span::new(5, 6))),
                Span::new(0, 6)
            ))
        );
    }
    #[ignore]
    #[test]
    fn test40() {
        assert_eq!(
            lang::statement("[1, 2, 3]"),
            Ok(Array(
                vec![
                    Integer(1, Span::new(1, 2)),
                    Integer(2, Span::new(4, 5)),
                    Integer(3, Span::new(7, 8))
                ],
                Span::new(0, 9)
            ))
        );
    }
}
