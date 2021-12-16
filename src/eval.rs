use crate::{
    ast::{Expr, Node, Statement},
    object::{BoolObject, IntObject, Object},
    token::Token,
};

#[derive(Debug)]
pub enum EvalError {}

pub fn eval<'a, N: Into<Node<'a>>>(node: N) -> Result<Object, EvalError> {
    let node: Node = node.into();

    match node {
        Node::Stmt(stmt) => match stmt {
            Statement::ExprStatement(expr_stmt) => {
                return eval(&expr_stmt.expr);
            }
            _ => {
                todo!()
            }
        },
        Node::Expr(expr) => match expr {
            Expr::Number(num_expr) => {
                return Ok(Object::Int(IntObject::new(num_expr.value)));
            }
            Expr::Bool(bool_expr) => {
                return Ok(Object::Bool(BoolObject::new(bool_expr.value)));
            }
            Expr::Prefix(prefix_expr) => match prefix_expr.op {
                Token::Bang => {
                    let obj = eval(&*prefix_expr.exp)?;
                    match obj {
                        Object::Int(v) => {
                            let val = if v.val == 0 { false } else { true };
                            return Ok(Object::Bool(BoolObject::new(!val)));
                        }
                        Object::Bool(v) => {
                            return Ok(Object::Bool(BoolObject::new(!v.val)));
                        }
                        _ => {
                            todo!()
                        }
                    }
                }
                _ => {
                    todo!()
                }
            },
            _ => {
                todo!()
            }
        },
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn eval_int() {
        let inputs = [("5", 5), ("10", 10)];

        for input in inputs {
            let object = eval_input(input.0);
            check_int_object(&object, input.1);
        }
    }

    #[test]
    fn eval_bool() {
        let inputs = [("true", true), ("false", false)];

        for input in inputs {
            let object = eval_input(input.0);
            check_bool_object(&object, input.1);
        }
    }

    #[test]
    fn eval_prefix_op() {
        let inputs = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!5", true),
        ];

        for input in inputs {
            let object = eval_input(input.0);
            check_bool_object(&object, input.1);
        }
    }

    fn check_int_object(object: &Object, value: i32) {
        match object {
            Object::Int(int_object) => {
                assert_eq!(int_object.val, value);
            }
            _ => {
                panic!("expected int object, but {:?}", object);
            }
        }
    }

    fn check_bool_object(object: &Object, value: bool) {
        match object {
            Object::Bool(bool_object) => {
                assert_eq!(bool_object.val, value);
            }
            _ => {
                panic!("expected bool object, but {:?}", object);
            }
        }
    }

    fn eval_input(input: &str) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let mut stmts = parser.parse_statements().unwrap();
        let stmt = stmts.remove(0);
        eval(&stmt).unwrap()
    }
}
