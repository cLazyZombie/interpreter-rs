use snafu::{Backtrace, Snafu};

use crate::{
    ast::{self, CallExpr, Expr, FuncExpr, InfixExpr, Statement},
    lexer::Lexer,
    token::{IdentToken, Token},
};

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

#[derive(Debug, Snafu)]
pub enum ParseError {
    NormalError { msg: String, backtrace: Backtrace },
    NoMoreToken { msg: String, backtrace: Backtrace },
    UnexpectedToken { msg: String, backtrace: Backtrace },
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // < or >
    Sum,         //+
    Product,     // *
    Prefix,      // -x or !x
    Call,        // any_function(a)
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let mut tokens = Vec::new();

        while let Some(token) = lexer.next_token() {
            if matches!(token, Token::EOF) {
                break;
            }
            tokens.push(token);
        }

        Self { tokens, cursor: 0 }
    }

    fn cur_token(&self) -> Option<Token> {
        self.tokens.get(self.cursor).cloned()
    }

    fn next_token(&self) -> Option<Token> {
        self.tokens.get(self.cursor + 1).cloned()
    }

    fn advancd_token(&mut self) {
        self.cursor += 1;
    }

    pub fn parse_program(mut self) -> Result<ast::Program, ParseError> {
        let statements = self.parse_statements()?;

        Ok(ast::Program::new(statements))
    }

    pub fn parse_statements(&mut self) -> Result<Vec<ast::Statement>, ParseError> {
        let mut statements = Vec::new();

        while let Some(token) = self.cur_token() {
            if matches!(token, Token::RBrace) {
                break;
            }

            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, ParseError> {
        if let Some(token) = self.cur_token() {
            let statement = match token {
                Token::Let => ast::Statement::LetStatement(self.parse_let_statement()?),
                Token::Return => ast::Statement::ReturnStatement(self.parse_return_statement()?),
                Token::LBrace => ast::Statement::BlockStatement(self.parse_block_statement()?),
                _ => ast::Statement::ExprStatement(self.parse_expr_statement()?),
            };
            Ok(statement)
        } else {
            NoMoreToken {
                msg: "to parse statement".to_string(),
            }
            .fail()
        }
    }

    fn parse_let_statement(&mut self) -> Result<ast::LetStatement, ParseError> {
        self.advancd_token(); // let

        let identifier = self.parse_identifier()?;

        // assign
        if !matches!(self.cur_token(), Some(Token::Assign)) {
            let err = format!("Token::Assign is expected, but {:?}", self.cur_token());
            return UnexpectedToken { msg: err }.fail();
        }
        self.advancd_token();

        let expr = self.parse_expr(Precedence::Lowest)?;

        let let_statement = ast::LetStatement::new(identifier, expr);
        Ok(let_statement)
    }

    fn parse_return_statement(&mut self) -> Result<ast::ReturnStatement, ParseError> {
        self.advancd_token(); // return

        let expr = self.parse_expr(Precedence::Lowest)?;
        let return_stmt = ast::ReturnStatement::new(expr);
        Ok(return_stmt)
    }

    fn parse_expr_statement(&mut self) -> Result<ast::ExprStatement, ParseError> {
        let expr = self.parse_expr(Precedence::Lowest)?;
        let expr_stmt = ast::ExprStatement::new(expr);
        Ok(expr_stmt)
    }

    fn parse_identifier(&mut self) -> Result<IdentToken, ParseError> {
        let id_token = self.cur_token().ok_or_else(|| {
            NoMoreToken {
                msg: "identifier token is missing".to_string(),
            }
            .build()
        })?;

        match id_token {
            Token::Ident(ident) => {
                let identifier = ident;
                self.advancd_token();
                Ok(identifier)
            }
            _ => {
                let err = format!("Token::Iden is needed, but {:?}", id_token);
                UnexpectedToken { msg: err }.fail()
            }
        }
    }

    fn parse_prefix_expr(&mut self) -> Result<Expr, ParseError> {
        let tok = self.cur_token().ok_or_else(|| {
            NoMoreToken {
                msg: "when parse prefix".to_string(),
            }
            .build()
        })?;

        let next_tok = self.next_token();

        let exp = match (tok, next_tok) {
            (tok, _) if tok.is_prefix_op() => {
                self.advancd_token();
                let exp = self.parse_expr(Precedence::Prefix)?;
                Expr::new_prefix_expr(tok, exp)
            }
            (Token::LParen, _) => self.parse_group_expr()?,
            (Token::If, _) => self.parse_if_expr()?,
            (Token::Function, _) => self.parse_function_expr()?,
            (tok, _) => {
                let result = Expr::new_single_expr(tok.clone()).ok_or_else(|| {
                    let err = format!("expr token expected, but {:?}", tok);
                    UnexpectedToken { msg: err }.build()
                })?;

                self.advancd_token();
                result
            }
        };

        Ok(exp)
    }

    fn parse_group_expr(&mut self) -> Result<Expr, ParseError> {
        self.expect_token(Token::LParen)?;

        let inner_expr = self.parse_expr(Precedence::Lowest)?;

        self.expect_token(Token::RParen)?;

        Ok(inner_expr)
    }

    fn parse_if_expr(&mut self) -> Result<Expr, ParseError> {
        self.expect_token(Token::If)?;

        let condition_expr = self.parse_group_expr()?;

        let consequence_statement =
            Box::new(Statement::BlockStatement(self.parse_block_statement()?));

        let alternative_statement = if let Ok(_else_token) = self.expect_token(Token::Else) {
            Some(Box::new(Statement::BlockStatement(
                self.parse_block_statement()?,
            )))
        } else {
            None
        };

        let if_expr = ast::IfExpr {
            condition: Box::new(condition_expr),
            consequence_statement,
            alternative_statement,
        };

        Ok(Expr::If(if_expr))
    }

    fn parse_function_expr(&mut self) -> Result<Expr, ParseError> {
        // fn
        self.expect_token(Token::Function)?;

        // name
        // let fn_name = self.parse_identifier()?;

        // args
        let mut args = Vec::new();
        self.expect_token(Token::LParen)?;
        loop {
            if self.cur_token() == Some(Token::RParen) {
                self.advancd_token();
                break;
            }

            if !args.is_empty() {
                self.expect_token(Token::Comma)?;
            }

            let arg = self.parse_identifier()?;
            args.push(arg);
        }

        // block statement
        let block_statement = self.parse_block_statement()?;

        let fn_expr = FuncExpr {
            args,
            body: Box::new(Statement::BlockStatement(block_statement)),
        };

        Ok(Expr::Function(fn_expr))
    }

    fn parse_block_statement(&mut self) -> Result<ast::BlockStatement, ParseError> {
        self.expect_token(Token::LBrace)?;

        let statements = self.parse_statements()?;

        self.expect_token(Token::RBrace)?;

        let block_statement = ast::BlockStatement { statements };

        Ok(block_statement)
    }

    fn expect_token(&mut self, expected_tok: Token) -> Result<Token, ParseError> {
        let cur_token = self.cur_token().ok_or_else(|| {
            let msg = format!("expectd {:?}, but no more token", expected_tok);
            NoMoreToken { msg }.build()
        })?;

        if cur_token != expected_tok {
            let msg = format!("expected {:?}, but {:?}", expected_tok, cur_token);
            return UnexpectedToken { msg }.fail();
        }

        self.advancd_token();
        Ok(cur_token)
    }

    fn parse_call_argument_expr(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut result = Vec::new();
        self.expect_token(Token::LParen)?;

        while let Some(tok) = self.cur_token() {
            if tok == Token::RParen {
                break;
            }

            if !result.is_empty() {
                self.expect_token(Token::Comma)?;
            }

            let expr = self.parse_expr(Precedence::Lowest)?;
            result.push(expr);
        }

        self.expect_token(Token::RParen)?;

        Ok(result)
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Result<Expr, ParseError> {
        let mut result = self.parse_prefix_expr()?;

        while let Some(cur_token) = self.cur_token() {
            // exit expr parsing if semicolon encountered
            if self.cur_token() == Some(Token::Semicolon) {
                self.advancd_token();
                return Ok(result);
            }

            if self.cur_token() == Some(Token::RBrace) {
                return Ok(result);
            }

            if cur_token.is_infix_op() {
                let cur_precedence = cur_token.precedence().unwrap(); // infix token should have precedencd
                if cur_precedence > precedence {
                    // function call
                    if cur_token == Token::LParen {
                        let args_expr = self.parse_call_argument_expr()?;
                        result = Expr::Call(CallExpr {
                            func: Box::new(result),
                            args: args_expr,
                        });
                        // result = self.parse_call_expr()?;
                    } else {
                        self.advancd_token();
                        let right_expr = self.parse_expr(cur_precedence)?;
                        result = Expr::Infix(InfixExpr::new(result, cur_token, right_expr));
                    }
                    continue;
                }
            }
            break;
        }

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        BlockStatement, CallExpr, ExprStatement, IfExpr, InfixExpr, PrefixExpr, Statement,
    };

    use super::*;

    #[test]
    fn let_statement() {
        let input = "let val = 10;";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        assert_eq!(program.statement_count(), 1);

        check_let_statement(program.get_statement(0).unwrap(), "val");
    }

    #[test]
    fn return_statement() {
        let input = "return 0;";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        assert_eq!(program.statement_count(), 1);

        check_return_statement(program.get_statement(0).unwrap());
    }

    #[test]
    fn boolean_expr() {
        let input = "true;";
        let stmts = input_to_statements(input);
        let expr = get_expr_statement(&stmts[0]).unwrap();
        check_bool_expr(&expr.expr, true);
    }

    #[test]
    fn expr_statement() {
        let input = "a;";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        assert_eq!(program.statement_count(), 1);
        check_expr_statement(program.get_statement(0).unwrap());
        assert_eq!(program.get_statement(0).unwrap().to_string(), "a;");
    }

    #[test]
    fn number_expr_statement() {
        let input = "6;";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();

        assert_eq!(program.statement_count(), 1);
        check_expr_statement(program.get_statement(0).unwrap());
        assert_eq!(program.get_statement(0).unwrap().to_string(), "6;");
    }

    #[test]
    fn statement_to_string() {
        let input = r#"
            let a = 10; a;
            return a;
        "#;

        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        assert_eq!(program.get_statement(0).unwrap().to_string(), "let a = 10;");
        assert_eq!(program.get_statement(1).unwrap().to_string(), "a;");
        assert_eq!(program.get_statement(2).unwrap().to_string(), "return a;");
    }

    #[test]
    fn infix_expr() {
        let input = "1 + 2;";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let expr_statement = get_expr_statement(program.get_statement(0).unwrap()).unwrap();

        let infix_expr = get_infix_expr(&expr_statement.expr).unwrap();
        check_number_expr(&infix_expr.left, 1);
        assert_eq!(infix_expr.op, Token::Plus);
        check_number_expr(&infix_expr.right, 2);
    }

    #[test]
    fn multiple_infix_expr() {
        let input = "1 + 2 - 3;";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let expr_statement = get_expr_statement(program.get_statement(0).unwrap()).unwrap();

        let infix_expr = get_infix_expr(&expr_statement.expr).unwrap();
        let left_infix_expr = get_infix_expr(&infix_expr.left).unwrap();
        check_number_expr(&left_infix_expr.left, 1);
        assert_eq!(left_infix_expr.op, Token::Plus);
        check_number_expr(&left_infix_expr.right, 2);

        assert_eq!(infix_expr.op, Token::Minus);
        check_number_expr(&infix_expr.right, 3);
    }

    #[test]
    fn prefix_operator_expr() {
        let input = "-1;!a;";
        let stmts = input_to_statements(input);
        assert_eq!(stmts.len(), 2);

        let expr_statement = get_expr_statement(&stmts[0]).unwrap();
        let prefix_expr = get_prefix_expr(&expr_statement.expr).unwrap();
        assert_eq!(prefix_expr.op, Token::Minus);
        check_number_expr(&prefix_expr.exp, 1);

        let expr_statement = get_expr_statement(&stmts[1]).unwrap();
        let prefix_expr = get_prefix_expr(&expr_statement.expr).unwrap();
        assert_eq!(prefix_expr.op, Token::Bang);
        check_identifier_expr(&prefix_expr.exp, "a");
    }

    #[test]
    fn test_to_string() {
        let input = [
            ("1 + 2 + 3;", "((1 + 2) + 3);"),
            ("1 + 2 * 3;", "(1 + (2 * 3));"),
            ("-1 + -2 / 3;", "(-1 + (-2 / 3));"),
            ("true;", "true;"),
        ];

        for i in input {
            let stmts = input_to_statements(i.0);
            assert_eq!(&stmts[0].to_string(), i.1);
        }
    }

    #[test]
    fn test_group_expr() {
        // let input = "a * (b + c) == true;";
        let input = "a * (b + c);";
        let stmts = input_to_statements(input);
        let stmt = get_expr_statement(stmts.get(0).unwrap()).unwrap();
        assert_eq!(stmt.to_string(), "(a * (b + c));");
        let left = get_infix_expr(&stmt.expr).unwrap();

        check_identifier_expr(&left.left, "a");
        let right = get_infix_expr(&left.right).unwrap();
        check_identifier_expr(&right.left, "b");
        check_identifier_expr(&right.right, "c");
    }

    #[test]
    fn test_equal_not_equal() {
        let input = "a == b;";
        let stmts = input_to_statements(input);
        let expr_stmt = get_expr_statement(&stmts[0]).unwrap();

        let infix = get_infix_expr(&expr_stmt.expr).unwrap();
        check_identifier_expr(&infix.left, "a");
        assert_eq!(infix.op, Token::Eq);
        check_identifier_expr(&infix.right, "b");
    }

    #[test]
    fn test_if_expr() {
        let input = "if (a == 0) { a };";
        let stmts = input_to_statements(input);
        let stmt = get_expr_statement(&stmts[0]).unwrap();

        let if_expr = get_if_expr(&stmt.expr).unwrap();
        let condition = get_infix_expr(&if_expr.condition).unwrap();
        check_identifier_expr(&condition.left, "a");
        assert_eq!(condition.op, Token::Eq);
        check_number_expr(&condition.right, 0);

        let consequence = &if_expr.consequence_statement;
        let consequence = get_block_statement(&consequence).unwrap();
        assert_eq!(consequence.statements.len(), 1);
        let expr = get_expr_statement(&consequence.statements[0]).unwrap();
        check_identifier_expr(&expr.expr, "a");
    }

    #[test]
    fn test_if_else_expr() {
        let input = "if (a == 0) { a } else { b };";
        let stmts = input_to_statements(input);
        let stmt = get_expr_statement(&stmts[0]).unwrap();

        let if_expr = get_if_expr(&stmt.expr).unwrap();
        let condition = get_infix_expr(&if_expr.condition).unwrap();
        check_identifier_expr(&condition.left, "a");
        assert_eq!(condition.op, Token::Eq);
        check_number_expr(&condition.right, 0);

        let consequence = &if_expr.consequence_statement;
        let consequence = get_block_statement(&consequence).unwrap();
        assert_eq!(consequence.statements.len(), 1);
        let expr = get_expr_statement(&consequence.statements[0]).unwrap();
        check_identifier_expr(&expr.expr, "a");

        let alternative = if_expr.alternative_statement.as_ref().unwrap();
        let alternative = get_block_statement(&alternative).unwrap();
        assert_eq!(alternative.statements.len(), 1);
        let expr = get_expr_statement(&alternative.statements[0]).unwrap();
        check_identifier_expr(&expr.expr, "b");
    }

    #[test]
    fn test_fn_expr() {
        let input = r#"
            fn() { return a + b; };
            fn(a) { return a + b; };
            fn (a, b, c) { return a + b; };
        "#;

        let stmts = input_to_statements(input);
        let expr_stmt = get_expr_statement(&stmts[0]).unwrap();
        check_fn_expr(&expr_stmt.expr, &[]);

        let expr_stmt = get_expr_statement(&stmts[1]).unwrap();
        check_fn_expr(&expr_stmt.expr, &["a"]);

        let expr_stmt = get_expr_statement(&stmts[2]).unwrap();
        check_fn_expr(&expr_stmt.expr, &["a", "b", "c"]);
    }

    #[test]
    fn test_fn_call() {
        let input = r#"
            my_func();
            my_func(1, 2);
            my_func2(my_func(1, 2), 3);
            fn (x) { x * 2} (10);
        "#;

        let stmts = input_to_statements(input);

        let stmt = get_expr_statement(&stmts[0]).unwrap();
        let call = get_call_expr(&stmt.expr).unwrap();
        assert_eq!(call.func.to_string(), "my_func");
        assert_eq!(call.args.len(), 0);

        let stmt = get_expr_statement(&stmts[1]).unwrap();
        let call = get_call_expr(&stmt.expr).unwrap();
        assert_eq!(call.func.to_string(), "my_func");
        assert_eq!(call.args.len(), 2);

        let stmt = get_expr_statement(&stmts[2]).unwrap();
        let call = get_call_expr(&stmt.expr).unwrap();
        assert_eq!(call.func.to_string(), "my_func2");
        assert_eq!(call.args.len(), 2);

        let stmt = get_expr_statement(&stmts[3]).unwrap();
        let call = get_call_expr(&stmt.expr).unwrap();
        assert_eq!(call.func.to_string(), "fn (x){\n(x * 2);\n}\n");
        assert_eq!(call.args.len(), 1);
    }

    fn get_call_expr(expr: &Expr) -> Option<&CallExpr> {
        match expr {
            Expr::Call(call_expr) => Some(call_expr),
            _ => None,
        }
    }

    fn check_fn_expr(expr: &Expr, args: &[&str]) {
        let fn_expr = get_function_expr(expr).unwrap();
        // assert_eq!(fn_expr.name.0, name);
        assert_eq!(fn_expr.args.len(), args.len());
        args.iter()
            .zip(&fn_expr.args)
            .for_each(|(a, b)| assert_eq!(*a, b.0));
    }

    fn check_let_statement(stmt: &Statement, name: &str) {
        match stmt {
            ast::Statement::LetStatement(let_stmt) => {
                assert_eq!(let_stmt.ident.0, name);
            }
            _ => panic!("not let statement"),
        }
    }

    fn check_expr_statement(stmt: &Statement) {
        match stmt {
            ast::Statement::ExprStatement(_expr) => {}
            _ => panic!("not expr statement"),
        }
    }

    fn get_if_expr(expr: &Expr) -> Option<&IfExpr> {
        match expr {
            Expr::If(if_expr) => Some(if_expr),
            _ => None,
        }
    }

    fn get_function_expr(expr: &Expr) -> Option<&FuncExpr> {
        match expr {
            Expr::Function(fn_expr) => Some(fn_expr),
            _ => None,
        }
    }

    fn get_expr_statement(statement: &Statement) -> Option<&ExprStatement> {
        match statement {
            Statement::ExprStatement(expr) => Some(expr),
            _ => None,
        }
    }

    fn get_block_statement(statement: &Statement) -> Option<&BlockStatement> {
        match statement {
            Statement::BlockStatement(block_stmt) => Some(block_stmt),
            _ => None,
        }
    }

    fn check_return_statement(stmt: &Statement) {
        match stmt {
            ast::Statement::ReturnStatement(_ret_stmt) => {}
            _ => panic!("not return statement"),
        }
    }

    fn get_prefix_expr(expr: &Expr) -> Option<&PrefixExpr> {
        match expr {
            Expr::Prefix(prefix) => Some(prefix),
            _ => None,
        }
    }

    fn get_infix_expr(expr: &Expr) -> Option<&InfixExpr> {
        match expr {
            Expr::Infix(infix) => Some(infix),
            _ => None,
        }
    }

    fn check_number_expr(expr: &Expr, value: i32) {
        match expr {
            Expr::Number(num) => {
                assert_eq!(num.value, value);
            }
            _ => panic!("expected number expr, but {:?}", expr),
        }
    }

    fn check_identifier_expr(expr: &Expr, name: &str) {
        match expr {
            Expr::Identifier(ident) => {
                assert_eq!(&ident.ident.0, name);
            }
            _ => panic!("expected identifier expr, but {:?}", expr),
        }
    }

    fn check_bool_expr(expr: &Expr, value: bool) {
        match expr {
            Expr::Bool(boolean) => {
                assert_eq!(boolean.value, value);
            }
            _ => panic!("expected boolean expr, but {:?}", expr),
        }
    }

    fn input_to_statements(input: &str) -> Vec<Statement> {
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        program.take_statement()
    }
}
