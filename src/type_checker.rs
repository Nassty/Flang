use crate::parser::Expression;
use crate::Span;
use std::collections::HashMap;

#[derive(Debug)]
pub struct TCError {
    pub message: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    args: Vec<InferedVal>,
    arg_names: Vec<String>,
    return_type: Box<InferedVal>,
}

#[derive(Debug)]
pub struct TypeChecker {
    values: HashMap<String, InferedVal>,
    classes: HashMap<String, HashMap<String, InferedVal>>,
}

#[derive(Debug, Clone, PartialEq)]
enum InferedVal {
    Integer,
    Float,
    Bool,
    String,
    Array(Vec<InferedVal>),
    Instance,
    Function(Function),
    Unit,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut s = Self {
            values: HashMap::new(),
            classes: HashMap::new(),
        };
        s.values.insert(
            "print".to_string(),
            InferedVal::Function(Function {
                args: vec![InferedVal::String],
                arg_names: vec!["s".to_string()],
                return_type: Box::new(InferedVal::Unit),
            }),
        );
        s
    }

    pub fn check(&mut self, expr: &Expression) -> Result<(), TCError> {
        match self.check_expr(expr) {
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        }
    }
    fn check_expr(&mut self, expr: &Expression) -> Result<InferedVal, TCError> {
        match expr {
            Expression::Return(expr, _) => self.check_expr(expr),
            Expression::Member(head, tail, _) => {
                let _head = self.check_expr(head)?;
                let tail = self.check_expr(tail)?;
                Ok(tail)
            }
            Expression::FunctionDeclaration(identifier, args, body, _) => {
                let name = match identifier.as_ref() {
                    Expression::Identifier(name, _) => name,
                    _ => todo!(),
                };
                let mut args_types = Vec::new();
                let mut arg_names = Vec::new();
                for arg in args {
                    if let Expression::Identifier(name, _) = arg {
                        arg_names.push(name.to_string());
                    }
                    let arg_type = InferedVal::Unit;
                    args_types.push(arg_type);
                }
                let mut return_type = InferedVal::Unit;
                for expr in body {
                    return_type = self.check_expr(expr)?;
                }
                self.values.insert(
                    name.clone(),
                    InferedVal::Function(Function {
                        args: args_types,
                        arg_names,
                        return_type: Box::new(return_type),
                    }),
                );
                Ok(InferedVal::Unit)
            }
            Expression::ClassDeclaration(head, body, _) => {
                if let Expression::Identifier(name, _) = head.as_ref() {
                    let constructor = Function {
                        args: Vec::new(),
                        arg_names: Vec::new(),
                        return_type: Box::new(InferedVal::Instance),
                    };
                    self.values
                        .insert(name.clone(), InferedVal::Function(constructor));
                    let mut class = HashMap::new();
                    for expr in body {
                        match expr {
                            Expression::Block(expressions, _) => {
                                for expression in expressions {
                                    match expression {
                                        Expression::FunctionDeclaration(
                                            identifier,
                                            args,
                                            body,
                                            _,
                                        ) => {
                                            let name = match identifier.as_ref() {
                                                Expression::Identifier(name, _) => name,
                                                _ => todo!(),
                                            };
                                            let mut args_types = Vec::new();
                                            let mut arg_names = Vec::new();
                                            for arg in args {
                                                if let Expression::Identifier(name, _) = arg {
                                                    arg_names.push(name.to_string());
                                                }
                                                let arg_type = InferedVal::Unit;
                                                args_types.push(arg_type);
                                            }
                                            let mut return_type = InferedVal::Unit;
                                            let old_values = self.values.clone();
                                            arg_names
                                                .iter()
                                                .zip(args_types.iter().cloned())
                                                .for_each(|(k, v)| {
                                                    self.values.insert(k.to_string(), v);
                                                });

                                            for expr in body {
                                                return_type = self.check_expr(expr)?;
                                            }
                                            class.insert(
                                                name.clone(),
                                                InferedVal::Function(Function {
                                                    args: args_types,
                                                    arg_names,
                                                    return_type: Box::new(return_type),
                                                }),
                                            );
                                            self.values = old_values;
                                        }
                                        _ => todo!(),
                                    }
                                }
                            }
                            k => todo!("{:?}", k),
                        }
                    }

                    self.classes.insert(name.clone(), class);
                }

                Ok(InferedVal::Unit)
            }
            Expression::Index(lhs, rhs, _) => {
                let lhs = self.check_expr(lhs)?;
                let rhs = self.check_expr(rhs)?;
                match (lhs, rhs) {
                    (InferedVal::Array(l), InferedVal::Integer) => Ok(l[0].clone()),
                    (lhs, rhs) => Err(TCError {
                        message: format!("Can't index {:?} with {:?}", lhs, rhs),
                        span: expr.span().clone(),
                    }),
                }
            }
            Expression::FunctionCall(name, args, _) => {
                let name: String = match name.as_ref() {
                    Expression::Identifier(name, _) => name.to_string(),
                    Expression::Member(_, _, _) => {
                        return Ok(InferedVal::Unit);
                    }
                    k => todo!("{:?}", k),
                };
                let funs = self.values.clone();
                let f = funs.get(&name);
                let function = match f {
                    Some(InferedVal::Function(f)) => f,
                    _ => {
                        return Err(TCError {
                            message: format!("Unknown function: {:?}", name),
                            span: expr.span().clone(),
                        })
                    }
                };
                if function.args.len() != args.len() {
                    return Err(TCError {
                        message: format!(
                            "Function {:?} takes {:?} arguments, but {:?} were given",
                            name,
                            function.args.len(),
                            args.len()
                        ),
                        span: expr.span().clone(),
                    });
                }
                for (i, arg) in args.iter().enumerate() {
                    let arg_type = self.check_expr(arg)?;
                    if arg_type != function.args[i] {
                        return Err(TCError {
                            message: format!(
                                "Argument {:?} of function {:?} is of type {:?}, but should be of type {:?}",
                                i, name, arg_type, function.args[i]
                            ),
                            span: expr.span().clone(),
                        });
                    }
                }
                Ok(*function.return_type.clone())
            }
            Expression::Integer(_, _) => Ok(InferedVal::Integer),
            Expression::Float(_, _) => Ok(InferedVal::Float),
            Expression::Bool(_, _) => Ok(InferedVal::Bool),
            Expression::String(_, _) => Ok(InferedVal::String),
            Expression::Unit(_) => Ok(InferedVal::Unit),
            Expression::Identifier(name, _) => match self.values.get(name) {
                Some(val) => Ok(val.clone()),
                None => {
                    panic!();
                    Err(TCError {
                        message: format!("Unknown identifier: {}", name),
                        span: expr.span().clone(),
                    })
                }
            },
            Expression::Sum(lhs, rhs, _) => {
                let lhs = self.check_expr(lhs)?;
                let rhs = self.check_expr(rhs)?;
                match (lhs, rhs) {
                    (InferedVal::Integer, InferedVal::Integer) => Ok(InferedVal::Integer),
                    (InferedVal::Float, InferedVal::Float) => Ok(InferedVal::Float),
                    (InferedVal::Integer, InferedVal::Float) => Ok(InferedVal::Float),
                    (InferedVal::Float, InferedVal::Integer) => Ok(InferedVal::Float),
                    (InferedVal::String, InferedVal::String) => Ok(InferedVal::String),
                    (InferedVal::Array(l), InferedVal::Array(r)) => Ok(InferedVal::Array(
                        l.iter()
                            .zip(r.iter())
                            .map(|(l, r)| if l == r { l.clone() } else { InferedVal::Unit })
                            .collect(),
                    )),
                    (lhs, rhs) => Err(TCError {
                        message: format!("Can't add {:?} and {:?}", lhs, rhs),
                        span: expr.span().clone(),
                    }),
                }
            }
            Expression::Sub(lhs, rhs, _) => {
                let lhs = self.check_expr(lhs)?;
                let rhs = self.check_expr(rhs)?;
                match (lhs, rhs) {
                    (InferedVal::Integer, InferedVal::Integer) => Ok(InferedVal::Integer),
                    (InferedVal::Float, InferedVal::Float) => Ok(InferedVal::Float),
                    (InferedVal::Integer, InferedVal::Float) => Ok(InferedVal::Float),
                    (InferedVal::Float, InferedVal::Integer) => Ok(InferedVal::Float),
                    (lhs, rhs) => Err(TCError {
                        message: format!("Can't subtract {:?} and {:?}", lhs, rhs),
                        span: expr.span().clone(),
                    }),
                }
            }
            Expression::Pow(lhs, rhs, _) => {
                let lhs = self.check_expr(lhs)?;
                let rhs = self.check_expr(rhs)?;
                match (lhs, rhs) {
                    (InferedVal::Integer, InferedVal::Integer) => Ok(InferedVal::Integer),
                    (InferedVal::Float, InferedVal::Float) => Ok(InferedVal::Float),
                    (InferedVal::Integer, InferedVal::Float) => Ok(InferedVal::Float),
                    (InferedVal::Float, InferedVal::Integer) => Ok(InferedVal::Float),
                    (lhs, rhs) => Err(TCError {
                        message: format!("Can't raise {:?} to the power of {:?}", lhs, rhs),
                        span: expr.span().clone(),
                    }),
                }
            }
            Expression::Div(lhs, rhs, _) => {
                let lhs = self.check_expr(lhs)?;
                let rhs = self.check_expr(rhs)?;
                match (lhs, rhs) {
                    (InferedVal::Integer, InferedVal::Integer) => Ok(InferedVal::Integer),
                    (InferedVal::Float, InferedVal::Float) => Ok(InferedVal::Float),
                    (InferedVal::Integer, InferedVal::Float) => Ok(InferedVal::Float),
                    (InferedVal::Float, InferedVal::Integer) => Ok(InferedVal::Float),
                    (lhs, rhs) => Err(TCError {
                        message: format!("Can't divide {:?} by {:?}", lhs, rhs),
                        span: expr.span().clone(),
                    }),
                }
            }
            Expression::Mod(lhs, rhs, _) => {
                let lhs = self.check_expr(lhs)?;
                let rhs = self.check_expr(rhs)?;
                match (lhs, rhs) {
                    (InferedVal::Integer, InferedVal::Integer) => Ok(InferedVal::Integer),
                    (lhs, rhs) => Err(TCError {
                        message: format!("Can't mod {:?} by {:?}", lhs, rhs),
                        span: expr.span().clone(),
                    }),
                }
            }
            Expression::AddAssign(lhs, rhs, _) => {
                let lhs = self.check_expr(lhs)?;
                let rhs = self.check_expr(rhs)?;
                match (lhs, rhs) {
                    (InferedVal::Integer, InferedVal::Integer) => Ok(InferedVal::Integer),
                    (InferedVal::Float, InferedVal::Float) => Ok(InferedVal::Float),
                    (InferedVal::Integer, InferedVal::Float) => Ok(InferedVal::Float),
                    (InferedVal::Float, InferedVal::Integer) => Ok(InferedVal::Float),
                    (InferedVal::String, InferedVal::String) => Ok(InferedVal::String),
                    (InferedVal::Array(l), InferedVal::Array(_)) => {
                        Ok(InferedVal::Array(vec![l[0].clone()]))
                    }
                    (lhs, rhs) => Err(TCError {
                        message: format!("Can't add {:?} and {:?}", lhs, rhs),
                        span: expr.span().clone(),
                    }),
                }
            }
            Expression::Lt(lhs, rhs, _)
            | Expression::Gt(lhs, rhs, _)
            | Expression::Lte(rhs, lhs, _)
            | Expression::Gte(lhs, rhs, _) => {
                let lhs = self.check_expr(lhs)?;
                let rhs = self.check_expr(rhs)?;
                match (lhs, rhs) {
                    (InferedVal::Integer, InferedVal::Integer) => Ok(InferedVal::Bool),
                    (InferedVal::Float, InferedVal::Float) => Ok(InferedVal::Bool),
                    (InferedVal::Integer, InferedVal::Float) => Ok(InferedVal::Bool),
                    (InferedVal::Float, InferedVal::Integer) => Ok(InferedVal::Bool),
                    (InferedVal::String, InferedVal::String) => Ok(InferedVal::Bool),
                    (lhs, rhs) => Err(TCError {
                        message: format!("Can't compare {:?} and {:?}", lhs, rhs),
                        span: expr.span().clone(),
                    }),
                }
            }
            Expression::If(cond, then, els, _) => {
                let mut j = InferedVal::Unit;
                if let Some(s) = els {
                    j = self.check_expr(s)?;
                }
                let cond = self.check_expr(cond)?;
                let then = self.check_expr(then)?;
                match (cond, then, j) {
                    (InferedVal::Bool, InferedVal::Integer, InferedVal::Integer) => {
                        Ok(InferedVal::Integer)
                    }
                    (InferedVal::Bool, InferedVal::Float, InferedVal::Float) => {
                        Ok(InferedVal::Float)
                    }
                    (InferedVal::Bool, InferedVal::String, InferedVal::String) => {
                        Ok(InferedVal::String)
                    }
                    (InferedVal::Bool, InferedVal::Bool, InferedVal::Bool) => Ok(InferedVal::Bool),
                    (InferedVal::Bool, InferedVal::Unit, InferedVal::Unit) => Ok(InferedVal::Unit),
                    (InferedVal::Bool, InferedVal::Array(l), InferedVal::Array(_)) => {
                        Ok(InferedVal::Array(vec![l[0].clone()]))
                    }
                    (cond, then, els) => Err(TCError {
                        message: format!(
                            "Can't use {:?} as a condition, {:?} as then and {:?} as else",
                            cond, then, els
                        ),
                        span: expr.span().clone(),
                    }),
                }
            }
            Expression::Block(x, _) => {
                let mut last = InferedVal::Unit;
                for e in x {
                    last = self.check_expr(e)?;
                }
                Ok(last)
            }
            Expression::Assign(head, val, _) => {
                let name = match head.as_ref() {
                    Expression::Identifier(name, _) => name,
                    _ => todo!(),
                };
                let val = self.check_expr(val)?;
                self.values.insert(name.to_string(), val);
                Ok(InferedVal::Unit)
            }
            Expression::Array(x, _) => {
                let mut vals = Vec::new();
                for e in x {
                    vals.push(self.check_expr(e)?);
                }
                Ok(InferedVal::Array(vals))
            }
            Expression::Mul(lhs, rhs, _) => {
                let lhs = self.check_expr(lhs)?;
                let rhs = self.check_expr(rhs)?;
                match (lhs, rhs) {
                    (InferedVal::Integer, InferedVal::Integer) => Ok(InferedVal::Integer),
                    (InferedVal::Float, InferedVal::Float) => Ok(InferedVal::Float),
                    (InferedVal::Integer, InferedVal::Float) => Ok(InferedVal::Float),
                    (InferedVal::Float, InferedVal::Integer) => Ok(InferedVal::Float),
                    (lhs, rhs) => Err(TCError {
                        message: format!("Can't multiply {:?} and {:?}", lhs, rhs),
                        span: expr.span().clone(),
                    }),
                }
            }
            Expression::While(cond, body, _) => {
                let cond = self.check_expr(cond)?;
                let body = self.check_expr(body)?;
                match (cond, body) {
                    (InferedVal::Bool, _) => Ok(InferedVal::Unit),
                    (cond, body) => Err(TCError {
                        message: format!(
                            "Can't use {:?} as a condition and {:?} as body",
                            cond, body
                        ),
                        span: expr.span().clone(),
                    }),
                }
            }
            Expression::SubAssign(lhs, rhs, _) => {
                let lhs = self.check_expr(lhs)?;
                let rhs = self.check_expr(rhs)?;
                match (lhs, rhs) {
                    (InferedVal::Integer, InferedVal::Integer) => Ok(InferedVal::Integer),
                    (InferedVal::Float, InferedVal::Float) => Ok(InferedVal::Float),
                    (InferedVal::Integer, InferedVal::Float) => Ok(InferedVal::Float),
                    (InferedVal::Float, InferedVal::Integer) => Ok(InferedVal::Float),
                    (lhs, rhs) => Err(TCError {
                        message: format!("Can't multiply {:?} and {:?}", lhs, rhs),
                        span: expr.span().clone(),
                    }),
                }
            }
            Expression::Incr(x, _) | Expression::Decr(x, _) => {
                let name = match x.as_ref() {
                    Expression::Identifier(name, _) => name,
                    _ => todo!(),
                };
                let val = self.values.get(name).ok_or(TCError {
                    message: format!("Can't find variable {}", name),
                    span: expr.span().clone(),
                })?;
                match val {
                    InferedVal::Integer => Ok(InferedVal::Integer),
                    InferedVal::Float => Ok(InferedVal::Float),
                    _ => Err(TCError {
                        message: format!("Can't increment {:?}", val),
                        span: expr.span().clone(),
                    }),
                }
            }
            Expression::And(lhs, rhs, _) | Expression::Or(lhs, rhs, _) => {
                let lhs = self.check_expr(lhs)?;
                let rhs = self.check_expr(rhs)?;
                match (lhs, rhs) {
                    (InferedVal::Bool, InferedVal::Bool) => Ok(InferedVal::Bool),
                    (lhs, rhs) => Err(TCError {
                        message: format!("Can't use {:?} and {:?}", lhs, rhs),
                        span: expr.span().clone(),
                    }),
                }
            }
            Expression::Eq(lhs, rhs, _) | Expression::Neq(lhs, rhs, _) => {
                let lhs = self.check_expr(lhs)?;
                let rhs = self.check_expr(rhs)?;
                match (lhs, rhs) {
                    (InferedVal::Integer, InferedVal::Integer) => Ok(InferedVal::Bool),
                    (InferedVal::Float, InferedVal::Float) => Ok(InferedVal::Bool),
                    (InferedVal::Integer, InferedVal::Float) => Ok(InferedVal::Bool),
                    (InferedVal::Float, InferedVal::Integer) => Ok(InferedVal::Bool),
                    (InferedVal::String, InferedVal::String) => Ok(InferedVal::Bool),
                    (InferedVal::Bool, InferedVal::Bool) => Ok(InferedVal::Bool),
                    (InferedVal::Unit, InferedVal::Unit) => Ok(InferedVal::Bool),
                    (InferedVal::Array(l), InferedVal::Array(_)) => {
                        Ok(InferedVal::Array(vec![l[0].clone()]))
                    }
                    (lhs, rhs) => Err(TCError {
                        message: format!("Can't compare {:?} and {:?}", lhs, rhs),
                        span: expr.span().clone(),
                    }),
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::Expression;
    #[test]
    fn test01() {
        let mut tc = TypeChecker::new();
        let expr = Expression::Integer(1, Span::new(0, 0));
        let res = tc.check_expr(&expr);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), InferedVal::Integer);
    }
    #[test]
    fn test02() {
        let mut tc = TypeChecker::new();
        let expr = Expression::Float(1.0, Span::new(0, 0));
        let res = tc.check_expr(&expr);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), InferedVal::Float);
    }
    #[test]
    fn test03() {
        let mut tc = TypeChecker::new();
        let expr = Expression::Bool(true, Span::new(0, 0));
        let res = tc.check_expr(&expr);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), InferedVal::Bool);
    }
    #[test]
    fn test04() {
        let mut tc = TypeChecker::new();
        let expr = Expression::String("foo".to_string(), Span::new(0, 0));
        let res = tc.check_expr(&expr);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), InferedVal::String);
    }
    #[test]
    fn test05() {
        let mut tc = TypeChecker::new();
        let expr = Expression::Unit(Span::new(0, 0));
        let res = tc.check_expr(&expr);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), InferedVal::Unit);
    }
    #[test]
    fn test06() {
        let mut tc = TypeChecker::new();
        let expr = Expression::Identifier("foo".to_string(), Span::new(0, 0));
        let res = tc.check_expr(&expr);
        assert!(res.is_err());
    }
    #[test]
    fn test07() {
        let mut tc = TypeChecker::new();
        let expr = crate::parser("a = 1;\na;").unwrap();
        let res = tc.check_expr(&expr).unwrap();
        assert_eq!(res, InferedVal::Integer);
    }
    #[test]
    fn test08() {
        let mut tc = TypeChecker::new();
        let expr = crate::parser("a=1;\nb=2;\na == b;").unwrap();
        let res = tc.check_expr(&expr).unwrap();
        assert_eq!(res, InferedVal::Bool);
    }
    #[test]
    fn test09() {
        let mut tc = TypeChecker::new();
        let expr = crate::parser("a=1;\nb=2;\na != b;").unwrap();
        let res = tc.check_expr(&expr).unwrap();
        assert_eq!(res, InferedVal::Bool);
    }
    #[test]
    fn test10() {
        let mut tc = TypeChecker::new();
        let expr = crate::parser("a=1;\nb=2;\na <= b;").unwrap();
        let res = tc.check_expr(&expr).unwrap();
        assert_eq!(res, InferedVal::Bool);
    }
    #[test]
    fn test11() {
        let mut tc = TypeChecker::new();
        let expr = crate::parser("a=1;\nb=2;\na >= b;").unwrap();
        let res = tc.check_expr(&expr).unwrap();
        assert_eq!(res, InferedVal::Bool);
    }
    #[test]
    fn test12() {
        let mut tc = TypeChecker::new();
        let expr = crate::parser("1+2;").unwrap();
        let res = tc.check_expr(&expr).unwrap();
        assert_eq!(res, InferedVal::Integer);
    }
    #[test]
    fn test13() {
        let mut tc = TypeChecker::new();
        let expr = crate::parser("1.0+2.0;").unwrap();
        let res = tc.check_expr(&expr).unwrap();
        assert_eq!(res, InferedVal::Float);
    }
    #[test]
    fn test14() {
        let mut tc = TypeChecker::new();
        let expr = crate::parser("1+1.0;").unwrap();
        let res = tc.check_expr(&expr).unwrap();
        assert_eq!(res, InferedVal::Float);
    }
    #[test]
    fn test15() {
        let mut tc = TypeChecker::new();
        let expr = crate::parser("1.0+1;").unwrap();
        let res = tc.check_expr(&expr).unwrap();
        assert_eq!(res, InferedVal::Float);
    }
    #[test]
    fn test16() {
        let mut tc = TypeChecker::new();
        let expr = crate::parser("\"a\"+\"b\";").unwrap();
        let res = tc.check_expr(&expr).unwrap();
        assert_eq!(res, InferedVal::String);
    }
    #[test]
    fn test17() {
        let mut tc = TypeChecker::new();
        let expr = crate::parser("[1, 2] + [3, 4];").unwrap();
        let res = tc.check_expr(&expr).unwrap();
        match res {
            InferedVal::Array(v) => {
                assert_eq!(v.len(), 2);
                assert_eq!(v[0], InferedVal::Integer);
                assert_eq!(v[1], InferedVal::Integer);
            }
            k => panic!("unexpected: {:?}", k),
        }
    }
    #[test]
    fn test18() {
        let mut tc = TypeChecker::new();
        let expr = crate::parser("[1, 2] + [3.0, 4.0];").unwrap();
        let res = tc.check_expr(&expr).unwrap();
        match res {
            InferedVal::Array(v) => {
                assert_eq!(v.len(), 2);
                assert_eq!(v[0], InferedVal::Unit);
                assert_eq!(v[1], InferedVal::Unit);
            }
            k => panic!("unexpected: {:?}", k),
        }
    }
}
