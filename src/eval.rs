use crate::{
    ast::{Expr, Node, Statement},
    object::{BoolObject, IntObject, Object, ReturnObject},
    token::Token,
};

#[derive(Debug)]
pub enum EvalError {
    FailedToConvertBoolError { original_type: String },
    FailedToConvertIntError { original_type: String },
    InvalidInfixOperator { operator_token: String },
    Todo,
}

pub fn eval<'a, N: Into<Node<'a>>>(node: N) -> Result<Object, EvalError> {
    let node: Node = node.into();

    match node {
        Node::Stmt(stmt) => match stmt {
            Statement::ExprStatement(expr_stmt) => eval(&expr_stmt.expr),
            Statement::BlockStatement(block_stmt) => {
                let mut result = Object::Null;

                // statement
                for stmt in &block_stmt.statements {
                    result = eval(stmt)?;

                    if let Object::Return(_) = &result {
                        return Ok(result);
                    }
                }

                Ok(result)
            }
            Statement::ReturnStatement(ret_stmt) => {
                let inner = eval(&ret_stmt.expr)?;
                let ret = ReturnObject::new(inner);
                Ok(ret.into())
            }
            _ => {
                eprintln!("stmt: {:?}", stmt);
                todo!()
            }
        },
        Node::Expr(expr) => match expr {
            Expr::Number(num_expr) => Ok(Object::Int(IntObject::new(num_expr.value))),
            Expr::Bool(bool_expr) => Ok(Object::Bool(BoolObject::new(bool_expr.value))),
            Expr::Prefix(prefix_expr) => match prefix_expr.op {
                Token::Bang => {
                    let obj = eval(&*prefix_expr.exp)?;
                    let bool_obj: BoolObject = obj.try_into()?;
                    Ok(bool_obj.bang().into())
                }
                Token::Minus => {
                    let obj = eval(&*prefix_expr.exp)?;
                    let int_obj: IntObject = obj.try_into()?;
                    Ok(int_obj.negate().into())
                }
                _ => {
                    todo!()
                }
            },
            Expr::If(if_expr) => {
                let cond = eval(&*if_expr.condition)?;
                let bool_cond: BoolObject = cond.try_into()?;
                if bool_cond.val {
                    eval(&*if_expr.consequence_statement)
                } else {
                    if let Some(alternative) = &if_expr.alternative_statement {
                        eval(&**alternative)
                    } else {
                        Ok(Object::Null)
                    }
                }
            }
            Expr::Infix(infix_expr) => {
                let left = eval(&*infix_expr.left)?;
                let right = eval(&*infix_expr.right)?;
                match infix_expr.op {
                    Token::Plus => {
                        let value = left.plus(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    Token::Minus => {
                        let value = left.minus(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    Token::Asterrisk => {
                        let value = left.asterrisk(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    Token::Slash => {
                        let value = left.slash(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    Token::Eq => {
                        let value = left.eq(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    Token::NotEq => {
                        let value = left.not_eq(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    Token::LT => {
                        let value = left.lt(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    Token::LTEq => {
                        let value = left.lt_eq(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    Token::GT => {
                        let value = left.gt(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    Token::GTEq => {
                        let value = left.gt_eq(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    _ => Err(EvalError::InvalidInfixOperator {
                        operator_token: infix_expr.op.to_string(),
                    }),
                }
            }
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
    fn eval_prefix_bang() {
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

    #[test]
    fn eval_infix_num_ops() {
        let inputs = [
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for input in inputs {
            let object = eval_input(input.0);
            check_int_object(&object, input.1);
        }
    }

    #[test]
    fn eval_prefix_minus() {
        let inputs = [("5", 5), ("-5", -5), ("10", 10), ("-10", -10)];

        for input in inputs {
            let object = eval_input(input.0);
            check_int_object(&object, input.1);
        }
    }

    #[test]
    fn eval_equal_not_equal() {
        let inputs = [
            ("1 == 2", false),
            ("1 == 1", true),
            ("1 != 2", true),
            ("1 != 1", false),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
        ];

        for input in inputs {
            let object = eval_input(input.0);
            check_bool_object(&object, input.1);
        }
    }

    #[test]
    fn eval_less_greater() {
        let inputs = [
            ("1 < 2", true),
            ("1 < 1", false),
            ("1 <= 1", true),
            ("1 > 2", false),
            ("1 > 1", false),
            ("1 >= 1", true),
        ];

        for input in inputs {
            let object = eval_input(input.0);
            check_bool_object(&object, input.1);
        }
    }

    #[test]
    fn eval_if() {
        let inputs = [
            ("if (1 < 2) { 1 } else { 2 };", 1),
            ("if (1 > 2) { 1 } else { 2 };", 2),
            ("if (1 > 2) { 1 } else { 2; 3; };", 3),
        ];

        for input in inputs {
            let object = eval_input(input.0);
            check_int_object(&object, input.1);
        }
    }

    #[test]
    fn eval_return() {
        let inputs = [
            ("{ 1; return 2; 3; }", 2),
            ("{ 1; { 2 } { return 3; } 4 }", 3),
        ];

        for input in inputs {
            let object = eval_input(input.0);
            check_int_object(&object, input.1);
        }
    }

    fn check_int_object(object: &Object, value: i32) {
        match object {
            Object::Int(int_object) => {
                assert_eq!(int_object.val, value);
            }
            Object::Return(ret_object) => check_int_object(&*ret_object.val, value),
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
            Object::Return(ret_object) => check_bool_object(&*ret_object.val, value),
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
