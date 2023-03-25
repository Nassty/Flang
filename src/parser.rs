use crate::Span;
use peg::parser;

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Index(Box<Expression>, Box<Expression>, Span),
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
    FunctionCall(Box<Expression>, Vec<Expression>, Span),
    ClassDeclaration(Box<Expression>, Vec<Expression>, Span),
    FunctionDeclaration(Box<Expression>, Vec<Expression>, Vec<Expression>, Span),
    Member(Box<Expression>, Box<Expression>, Span),
    If(
        Box<Expression>,
        Box<Expression>,
        Option<Box<Expression>>,
        Span,
    ),
    Return(Box<Expression>, Span),
}

impl Expression {
    pub fn span(&self) -> &Span {
        use Expression::*;
        match self {
            Return(_, span) => span,
            Member(_, _, span) => span,
            FunctionDeclaration(_, _, _, span) => span,
            ClassDeclaration(_, _, span) => span,
            FunctionCall(_, _, span) => span,
            Index(_, _, span) => span,
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
    rule statements() -> Expression
        = _ v:(statement())+ _ {
            let span = Span::new(v[0].span().start(), v[v.len()-1].span().end());
            Expression::Block(v, span)
        }
    rule comma_separated() -> Vec<Expression>
        = atom()  ** ","

    rule array() -> Expression
        = _ p1:position!() "[" _ v:comma_separated() _ "]" p2:position!() {
            let span = Span::new(p1, p2);
            Expression::Array(v, span)
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

    rule member() -> Expression
        = _ v0:identifier() "." v1:identifier() {
            let span = Span::new(v0.span().start(), v1.span().end());
            Expression::Member(Box::new(v0), Box::new(v1), span)
        }

    rule return_statement() -> Expression
        = _ p:position!() "return" _ v:expression() {
            let span = Span::new(p, v.span().end());
            Expression::Return(Box::new(v), span)
        }

    rule postfix() -> Expression
        = v:increment()
        / v:decrement()
        / v:member()
        / v:identifier()

    pub rule statement() -> Expression
        = x:expression() _ ";"  {
            x
        }/ x:assign()
        / if_else()
        / x:if_statement()
        / x:while_statement()
        / x:function_call() _ ";" {
            x
        }
        / x:class_decl() {
            x
        } / x:function_decl() {
            x
        }/ x:return_statement() _ ";" {
            x
        }

    pub rule expression() -> Expression
        = cmp()
        / sum()
        / postfix()


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


    rule index_assign() -> Expression
        = _ l:index() _ "=" _ r:statement() {
            let span = Span::new(l.span().start(), r.span().end());
            Expression::Assign(Box::new(l), Box::new(r), span)
        }

    rule assign() -> Expression
        = add_assign()
        / sub_assign()
        / index_assign()
        / _ l:identifier() _ "=" _ r:statement() {
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
        = _ "true" _ end:position!() {
            let start = end - 4;
            Expression::Bool(true, Span::new(start, end))
        }
        / _ "false" _  end:position!() {
            let start = end - 5;
            Expression::Bool(false, Span::new(start, end))
        }

    pub rule index() -> Expression
        = _ i:identifier() _ "[" _ b:atom() _ "]" _  {
            let span = Span::new(i.span().start(), i.span().end());
            Expression::Index(Box::new(i), Box::new(b), span)
        }

    pub rule function_call() -> Expression
        = _ p1:position!() i:identifier() _ "(" _ args:expression() ** ("," _ ) _ ")" _ p2:position!() {
            let span = Span::new(p1, p2);
            Expression::FunctionCall(Box::new(i), args, span)
        } /
        _ p1:position!() i:identifier() _ "(" _ ")" _ p2:position!() {
            let span = Span::new(p1, p2);
            Expression::FunctionCall(Box::new(i), vec![], span)
        } /
        _ p1:position!() i:member() _ "(" _ ")" _ p2:position!() {
            let span = Span::new(p1, p2);
            Expression::FunctionCall(Box::new(i), vec![], span)
        }
        / _ p1:position!() i:member () _ "(" _ args:expression() ** ("," _ ) _ ")" _ p2:position!() {
            let span = Span::new(p1, p2);
            Expression::FunctionCall(Box::new(i), args, span)
        }

    pub rule atom() -> Expression
        = float()
         / index()
         / array()
         / string()
         / integer()
         / unit()
         / string()
         / bool()
         / increment()
         / decrement()
         / postfix()
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
        = _ n:$(['0'..='9']+) "." m:$(['0'..='9']+) end:position!() {
            let start = end - n.len() - m.len() - 1;
            Expression::Float(format!("{}.{}", n, m).parse().unwrap(), Span::new(start, end))
        }

    rule integer() -> Expression
        =_  n:$(['0'..='9']+) end: position!() {
            let start = end - n.len();
            Expression::Integer(n.parse().unwrap(), Span::new(start, end))
        }

    rule class_body() -> Vec<Expression>
        = _ v:statements() ** _ { v }
        / _ { vec![] }
    rule class_decl() -> Expression
        = p1:position!() _ "class " _ i:identifier() _ "=>" _ c:class_body() _ "---" p2:position!()_ {

            Expression::ClassDeclaration(Box::new(i), c, Span::new(p1, p2))
        }

    rule function_body() -> Vec<Expression>
        = _ v:statements() ** _ { v }
        / _ { vec![] }

    rule function_decl() -> Expression
        = p1:position!() _ "fun " _ i:identifier() _  args:identifier() ** ("," _) _ "=>" _ c:function_body() _ "---" p2:position!() {

            Expression::FunctionDeclaration(Box::new(i), args, c, Span::new(p1, p2))
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
    #[test]
    fn test40() {
        assert_eq!(
            lang::statement("[1, 2, 3];"),
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
    #[test]
    fn test41() {
        assert_eq!(
            lang::statement("a[1];"),
            Ok(Index(
                Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                Box::new(Integer(1, Span::new(2, 3))),
                Span::new(0, 1)
            ))
        );
    }
    #[test]
    fn test42() {
        assert_eq!(
            lang::statement("a[1] = 2;"),
            Ok(Assign(
                Box::new(Index(
                    Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                    Box::new(Integer(1, Span::new(2, 3))),
                    Span::new(0, 1)
                )),
                Box::new(Integer(2, Span::new(7, 8))),
                Span::new(0, 8)
            ))
        );
    }
    #[test]
    fn test43() {
        assert_eq!(
            lang::statement("a();"),
            Ok(FunctionCall(
                Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                vec![],
                Span::new(0, 3)
            ))
        );
    }
    #[test]
    fn test44() {
        assert_eq!(
            lang::statement("a(1, 2, 3);"),
            Ok(FunctionCall(
                Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                vec![
                    Integer(1, Span::new(2, 3)),
                    Integer(2, Span::new(5, 6)),
                    Integer(3, Span::new(8, 9))
                ],
                Span::new(0, 10)
            ))
        );
    }
    #[test]
    fn test45() {
        assert_eq!(
            lang::parser("c = a();"),
            Ok(Block(
                vec![Assign(
                    Box::new(Identifier("c".to_string(), Span::new(0, 1))),
                    Box::new(FunctionCall(
                        Box::new(Identifier("a".to_string(), Span::new(4, 5))),
                        vec![],
                        Span::new(4, 7)
                    )),
                    Span::new(0, 7)
                )],
                Span::new(0, 7)
            ))
        );
    }
    #[test]
    fn test46() {
        assert_eq!(
            lang::parser("c = a(1, 2, 3);"),
            Ok(Block(
                vec![Assign(
                    Box::new(Identifier("c".to_string(), Span::new(0, 1))),
                    Box::new(FunctionCall(
                        Box::new(Identifier("a".to_string(), Span::new(4, 5))),
                        vec![
                            Integer(1, Span::new(6, 7)),
                            Integer(2, Span::new(9, 10)),
                            Integer(3, Span::new(12, 13))
                        ],
                        Span::new(4, 14)
                    )),
                    Span::new(0, 14)
                )],
                Span::new(0, 14)
            ))
        );
    }
    #[test]
    fn test47() {
        assert_eq!(
            lang::parser("[1.0, 2.0, 3.0];"),
            Ok(Block(
                vec![Array(
                    vec![
                        Float(1.0, Span::new(1, 4)),
                        Float(2.0, Span::new(6, 9)),
                        Float(3.0, Span::new(11, 14))
                    ],
                    Span::new(0, 15)
                )],
                Span::new(0, 15)
            ))
        );
    }
    #[test]
    fn test48() {
        assert_eq!(
            lang::parser("class A =>\n ---"),
            Ok(Block(
                vec![ClassDeclaration(
                    Box::new(Identifier("A".to_string(), Span::new(6, 7))),
                    vec![],
                    Span::new(0, 15)
                )],
                Span::new(0, 15)
            ))
        );
    }
    #[test]
    fn test49() {
        assert_eq!(
            lang::parser("fun a b, c => ---"),
            Ok(Block(
                vec![FunctionDeclaration(
                    Box::new(Identifier("a".to_string(), Span::new(4, 5))),
                    vec![
                        Identifier("b".to_string(), Span::new(6, 7)),
                        Identifier("c".to_string(), Span::new(9, 10))
                    ],
                    vec![],
                    Span::new(0, 17)
                )],
                Span::new(0, 17)
            ))
        );
    }
    #[test]
    fn test50() {
        assert_eq!(
            lang::parser("fun a b, c => a = 1; ---"),
            Ok(Block(
                vec![FunctionDeclaration(
                    Box::new(Identifier("a".to_string(), Span::new(4, 5))),
                    vec![
                        Identifier("b".to_string(), Span::new(6, 7)),
                        Identifier("c".to_string(), Span::new(9, 10))
                    ],
                    vec![Block(
                        vec![Assign(
                            Box::new(Identifier("a".to_string(), Span::new(14, 15))),
                            Box::new(Integer(1, Span::new(18, 19))),
                            Span::new(14, 19)
                        )],
                        Span::new(14, 19)
                    )],
                    Span::new(0, 24)
                )],
                Span::new(0, 24)
            ))
        );
    }
    #[test]
    fn test51() {
        assert_eq!(
            lang::parser("class A => fun a b, c => a = 1; --- ---"),
            Ok(Block(
                vec![ClassDeclaration(
                    Box::new(Identifier("A".to_string(), Span::new(6, 7))),
                    vec![Block(
                        vec![FunctionDeclaration(
                            Box::new(Identifier("a".to_string(), Span::new(15, 16))),
                            vec![
                                Identifier("b".to_string(), Span::new(17, 18)),
                                Identifier("c".to_string(), Span::new(20, 21))
                            ],
                            vec![Block(
                                vec![Assign(
                                    Box::new(Identifier("a".to_string(), Span::new(25, 26))),
                                    Box::new(Integer(1, Span::new(29, 30))),
                                    Span::new(25, 30)
                                )],
                                Span::new(25, 30)
                            )],
                            Span::new(11, 35)
                        )],
                        Span::new(11, 35)
                    )],
                    Span::new(0, 39)
                )],
                Span::new(0, 39)
            ))
        );
    }
    #[test]
    fn test52() {
        assert_eq!(
            lang::parser("a.b;"),
            Ok(Block(
                vec![Member(
                    Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                    Box::new(Identifier("b".to_string(), Span::new(2, 3))),
                    Span::new(0, 3)
                )],
                Span::new(0, 3)
            ))
        );
    }
    #[test]
    fn test53() {
        assert_eq!(
            lang::parser("a.b();"),
            Ok(Block(
                vec![FunctionCall(
                    Box::new(Member(
                        Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                        Box::new(Identifier("b".to_string(), Span::new(2, 3))),
                        Span::new(0, 3)
                    )),
                    vec![],
                    Span::new(0, 5)
                )],
                Span::new(0, 5)
            ))
        );
    }
    #[test]
    fn test54() {
        assert_eq!(
            lang::parser("a.b(1);"),
            Ok(Block(
                vec![FunctionCall(
                    Box::new(Member(
                        Box::new(Identifier("a".to_string(), Span::new(0, 1))),
                        Box::new(Identifier("b".to_string(), Span::new(2, 3))),
                        Span::new(0, 3)
                    )),
                    vec![Integer(1, Span::new(4, 5))],
                    Span::new(0, 6)
                )],
                Span::new(0, 6)
            ))
        );
    }
    #[test]
    fn test55() {
        assert_eq!(
            lang::parser("return a;"),
            Ok(Block(
                vec![Return(
                    Box::new(Identifier("a".to_string(), Span::new(7, 8))),
                    Span::new(0, 8)
                )],
                Span::new(0, 8)
            ))
        );
    }
}
