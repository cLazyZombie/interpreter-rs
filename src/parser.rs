use crate::{
    ast::{self, FunctionExpression},
    lexer::Lexer,
    token::Token,
};

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

#[derive(Debug)]
pub enum ParseError {
    NormalError(String),
    NoMoreToken(String),
    UnexpectedToken(String),
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

    fn _next_token(&self) -> Option<Token> {
        self.tokens.get(self.cursor + 1).cloned()
    }

    fn advancd_token(&mut self) {
        self.cursor += 1;
    }

    pub fn parse_program(mut self) -> Result<ast::Program, ParseError> {
        let statements = self.parse_statements()?;

        Ok(ast::Program::new(statements))
    }

    fn parse_statements(&mut self) -> Result<Vec<ast::Statement>, ParseError> {
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
                _ => ast::Statement::ExpressionStatement(self.parse_expression_statement()?),
            };
            Ok(statement)
        } else {
            Err(ParseError::NoMoreToken("to parse statement".to_string()))
        }
    }

    fn parse_let_statement(&mut self) -> Result<ast::LetStatement, ParseError> {
        self.advancd_token(); // let

        let identifier = self.parse_identifier()?;

        // assign
        if !matches!(self.cur_token(), Some(Token::Assign)) {
            let err = format!("Token::Assign is expected, but {:?}", self.cur_token());
            return Err(ParseError::UnexpectedToken(err));
        }
        self.advancd_token();

        let expression = self.parse_expression(Precedence::Lowest)?;

        let let_statement = ast::LetStatement::new(identifier, expression);
        Ok(let_statement)
    }

    fn parse_return_statement(&mut self) -> Result<ast::ReturnStatement, ParseError> {
        self.advancd_token(); // return

        let expression = self.parse_expression(Precedence::Lowest)?;
        let return_stmt = ast::ReturnStatement::new(expression);
        Ok(return_stmt)
    }

    fn parse_expression_statement(&mut self) -> Result<ast::ExpressionStatement, ParseError> {
        let expression = self.parse_expression(Precedence::Lowest)?;
        let expression_stmt = ast::ExpressionStatement::new(expression);
        Ok(expression_stmt)
    }

    fn parse_identifier(&mut self) -> Result<ast::Identifier, ParseError> {
        let id_token = self
            .cur_token()
            .ok_or_else(|| ParseError::NoMoreToken("identifier token is missing".to_string()))?;

        match id_token {
            Token::Iden(name) => {
                let identifier = ast::Identifier::new(name);
                self.advancd_token();
                Ok(identifier)
            }
            _ => {
                let err = format!("Token::Iden is needed, but {:?}", id_token);
                Err(ParseError::UnexpectedToken(err))
            }
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<ast::Expression, ParseError> {
        let tok = self
            .cur_token()
            .ok_or_else(|| ParseError::NoMoreToken("when parse prefix".to_string()))?;

        let exp = if tok.is_prefix_op() {
            self.advancd_token();
            let exp = self.parse_expression(Precedence::Prefix)?;
            ast::Expression::new_prefix_expression(tok, exp)
        } else if tok == Token::LParen {
            self.parse_group_expression()?
        } else if tok == Token::If {
            self.parse_if_expression()?
        } else if tok == Token::Function {
            self.parse_function_expression()?
        } else {
            let result = ast::Expression::new_single_expression(tok.clone()).ok_or_else(|| {
                let err = format!("expression token expected, but {:?}", tok);
                ParseError::UnexpectedToken(err)
            })?;

            self.advancd_token();
            result
        };

        Ok(exp)
    }

    fn parse_group_expression(&mut self) -> Result<ast::Expression, ParseError> {
        self.expect_token(Token::LParen)?;

        let inner_expression = self.parse_expression(Precedence::Lowest)?;

        self.expect_token(Token::RParen)?;

        Ok(inner_expression)
    }

    fn parse_if_expression(&mut self) -> Result<ast::Expression, ParseError> {
        self.expect_token(Token::If)?;

        let condition_expression = self.parse_group_expression()?;

        let consequence_statement = self.parse_block_statement()?;

        let alternative_statement = if let Ok(_else_token) = self.expect_token(Token::Else) {
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        let if_expression = ast::IfExpression {
            condition: Box::new(condition_expression),
            consequence_statement,
            alternative_statement,
        };

        Ok(ast::Expression::If(if_expression))
    }

    fn parse_function_expression(&mut self) -> Result<ast::Expression, ParseError> {
        // fn
        self.expect_token(Token::Function)?;

        // name
        let fn_name = self.parse_identifier()?;

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

        let fn_expression = FunctionExpression {
            name: fn_name,
            args,
            block_statement,
        };

        Ok(ast::Expression::Function(fn_expression))
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
            ParseError::NoMoreToken(msg)
        })?;

        if cur_token != expected_tok {
            let msg = format!("expected {:?}, but {:?}", expected_tok, cur_token);
            return Err(ParseError::UnexpectedToken(msg));
        }

        self.advancd_token();
        Ok(cur_token)
    }

    fn parse_call_argument_expression(&mut self) -> Result<Vec<ast::Expression>, ParseError> {
        eprintln!("############## parse_call_argument_expression");

        let mut result = Vec::new();
        self.expect_token(Token::LParen)?;

        while let Some(tok) = self.cur_token() {
            if tok == Token::RParen {
                break;
            }

            if !result.is_empty() {
                self.expect_token(Token::Comma)?;
            }

            let expression = self.parse_expression(Precedence::Lowest)?;
            result.push(expression);
        }

        self.expect_token(Token::RParen)?;

        Ok(result)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<ast::Expression, ParseError> {
        let mut result = self.parse_prefix_expression()?;

        // exit expression parsing if semicolon encountered
        if self.cur_token() == Some(Token::Semicolon) {
            self.advancd_token();
            return Ok(result);
        }

        if self.cur_token() == Some(Token::RBrace) {
            return Ok(result);
        }

        // if self.cur_token() == Some(Token::Comma) {
        //     return Ok(result);
        // }

        while let Some(cur_token) = self.cur_token() {
            if self.cur_token() == Some(Token::Semicolon) {
                self.advancd_token();
                return Ok(result);
            }

            if cur_token.is_infix_op() {
                let cur_precedence = cur_token.precedence().unwrap(); // infix token should have precedencd
                if cur_precedence > precedence {
                    let infix_expression = match cur_token {
                        Token::LParen => {
                            let args = self.parse_call_argument_expression()?;
                            if let ast::Expression::Identifier(identifier) = result {
                                ast::Expression::new_fn_call(identifier, args)
                            } else {
                                return Err(ParseError::UnexpectedToken(format!(
                                    "identifier is expected, but {:?}",
                                    result
                                )));
                            }
                        }
                        _ => {
                            self.advancd_token();
                            let right_expression = self.parse_expression(cur_precedence)?;

                            ast::Expression::new_infix_expression(
                                result,
                                cur_token,
                                right_expression,
                            )
                        }
                    };
                    result = infix_expression;
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
        CallExpression, Expression, ExpressionStatement, IfExpression, InfixExpression,
        PrefixExpression, Statement,
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
    fn boolean_expression() {
        let input = "true;";
        let stmts = input_to_statements(input);
        let expression = get_expression_statement(&stmts[0]).unwrap();
        check_bool_expression(&expression.expression, true);
    }

    #[test]
    fn expression_statement() {
        let input = "a;";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        assert_eq!(program.statement_count(), 1);
        check_expression_statement(program.get_statement(0).unwrap());
        assert_eq!(program.get_statement(0).unwrap().to_string(), "a;");
    }

    #[test]
    fn number_expression_statement() {
        let input = "6;";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();

        assert_eq!(program.statement_count(), 1);
        check_expression_statement(program.get_statement(0).unwrap());
        assert_eq!(program.get_statement(0).unwrap().to_string(), "6;");
    }

    #[test]
    fn statement_to_string() {
        let input = r#"
            let a = 10;
            a;
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
    fn infix_expression() {
        let input = "1 + 2;";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let expression_statement =
            get_expression_statement(program.get_statement(0).unwrap()).unwrap();

        let infix_expression = get_infix_expression(&expression_statement.expression).unwrap();
        check_number_expression(&infix_expression.left, 1);
        assert_eq!(infix_expression.op, Token::Plus);
        check_number_expression(&infix_expression.right, 2);
    }

    #[test]
    fn multiple_infix_expression() {
        let input = "1 + 2 - 3;";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let expression_statement =
            get_expression_statement(program.get_statement(0).unwrap()).unwrap();

        let infix_expression = get_infix_expression(&expression_statement.expression).unwrap();
        let left_infix_expression = get_infix_expression(&infix_expression.left).unwrap();
        check_number_expression(&left_infix_expression.left, 1);
        assert_eq!(left_infix_expression.op, Token::Plus);
        check_number_expression(&left_infix_expression.right, 2);

        assert_eq!(infix_expression.op, Token::Minus);
        check_number_expression(&infix_expression.right, 3);
    }

    #[test]
    fn prefix_operator_expression() {
        let input = "-1;!a;";
        let stmts = input_to_statements(input);
        assert_eq!(stmts.len(), 2);

        let expression_statement = get_expression_statement(&stmts[0]).unwrap();
        let prefix_expression = get_prefix_expression(&expression_statement.expression).unwrap();
        assert_eq!(prefix_expression.op, Token::Minus);
        check_number_expression(&prefix_expression.exp, 1);

        let expression_statement = get_expression_statement(&stmts[1]).unwrap();
        let prefix_expression = get_prefix_expression(&expression_statement.expression).unwrap();
        assert_eq!(prefix_expression.op, Token::Bang);
        check_identifier_expression(&prefix_expression.exp, "a");
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
    fn test_group_expression() {
        // let input = "a * (b + c) == true;";
        let input = "a * (b + c);";
        let stmts = input_to_statements(input);
        let stmt = get_expression_statement(stmts.get(0).unwrap()).unwrap();
        assert_eq!(stmt.to_string(), "(a * (b + c));");
        let left = get_infix_expression(&stmt.expression).unwrap();

        check_identifier_expression(&left.left, "a");
        let right = get_infix_expression(&left.right).unwrap();
        check_identifier_expression(&right.left, "b");
        check_identifier_expression(&right.right, "c");
    }

    #[test]
    fn test_equal_not_equal() {
        let input = "a == b;";
        let stmts = input_to_statements(input);
        let expression_stmt = get_expression_statement(&stmts[0]).unwrap();

        let infix = get_infix_expression(&expression_stmt.expression).unwrap();
        check_identifier_expression(&infix.left, "a");
        assert_eq!(infix.op, Token::Eq);
        check_identifier_expression(&infix.right, "b");
    }

    #[test]
    fn test_if_expression() {
        let input = "if (a == 0) { a };";
        let stmts = input_to_statements(input);
        let stmt = get_expression_statement(&stmts[0]).unwrap();

        let if_expression = get_if_expression(&stmt.expression).unwrap();
        let condition = get_infix_expression(&if_expression.condition).unwrap();
        check_identifier_expression(&condition.left, "a");
        assert_eq!(condition.op, Token::Eq);
        check_number_expression(&condition.right, 0);

        let consequence = &if_expression.consequence_statement;
        assert_eq!(consequence.statements.len(), 1);
        let expr = get_expression_statement(&consequence.statements[0]).unwrap();
        check_identifier_expression(&expr.expression, "a");
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (a == 0) { a } else { b };";
        let stmts = input_to_statements(input);
        let stmt = get_expression_statement(&stmts[0]).unwrap();

        let if_expression = get_if_expression(&stmt.expression).unwrap();
        let condition = get_infix_expression(&if_expression.condition).unwrap();
        check_identifier_expression(&condition.left, "a");
        assert_eq!(condition.op, Token::Eq);
        check_number_expression(&condition.right, 0);

        let consequence = &if_expression.consequence_statement;
        assert_eq!(consequence.statements.len(), 1);
        let expr = get_expression_statement(&consequence.statements[0]).unwrap();
        check_identifier_expression(&expr.expression, "a");

        let alternative = if_expression.alternative_statement.as_ref().unwrap();
        assert_eq!(alternative.statements.len(), 1);
        let expr = get_expression_statement(&alternative.statements[0]).unwrap();
        check_identifier_expression(&expr.expression, "b");
    }

    #[test]
    fn test_fn_expression() {
        let input = r#"
            fn my_func_1() { return a + b; };
            fn my_func_2(a) { return a + b; };
            fn my_func_3(a, b, c) { return a + b; };
        "#;

        let stmts = input_to_statements(input);
        let expression_stmt = get_expression_statement(&stmts[0]).unwrap();
        check_fn_expression(&expression_stmt.expression, "my_func_1", &[]);

        let expression_stmt = get_expression_statement(&stmts[1]).unwrap();
        check_fn_expression(&expression_stmt.expression, "my_func_2", &["a"]);

        let expression_stmt = get_expression_statement(&stmts[2]).unwrap();
        check_fn_expression(&expression_stmt.expression, "my_func_3", &["a", "b", "c"]);
    }

    #[test]
    fn test_fn_call() {
        // let input = r#"
        //     my_func();
        //     my_func(1, 2);
        //     my_func2(my_func(1, 2), 3);
        // "#;

        let input = "my_func();";

        let stmts = input_to_statements(input);

        let stmt = get_expression_statement(&stmts[0]).unwrap();
        let call = get_call_expression(&stmt.expression).unwrap();
        assert_eq!(call.fn_name.name, "my_func");
        assert_eq!(call.args.len(), 0);
    }

    fn get_call_expression(expression: &Expression) -> Option<&CallExpression> {
        match expression {
            Expression::Call(call_expression) => Some(call_expression),
            _ => None,
        }
    }

    fn check_fn_expression(expression: &Expression, name: &str, args: &[&str]) {
        let fn_expression = get_function_expression(expression).unwrap();
        assert_eq!(fn_expression.name.name, name);
        assert_eq!(fn_expression.args.len(), args.len());
        args.iter()
            .zip(&fn_expression.args)
            .for_each(|(a, b)| assert_eq!(*a, b.name));
    }

    fn check_let_statement(stmt: &Statement, name: &str) {
        match stmt {
            ast::Statement::LetStatement(let_stmt) => {
                assert_eq!(let_stmt.identifier.name, name.to_string());
            }
            _ => panic!("not let statement"),
        }
    }

    fn check_expression_statement(stmt: &Statement) {
        match stmt {
            ast::Statement::ExpressionStatement(_expr) => {}
            _ => panic!("not expression statement"),
        }
    }

    fn get_if_expression(expression: &Expression) -> Option<&IfExpression> {
        match expression {
            Expression::If(if_expression) => Some(if_expression),
            _ => None,
        }
    }

    fn get_function_expression(expression: &Expression) -> Option<&FunctionExpression> {
        match expression {
            Expression::Function(fn_expression) => Some(fn_expression),
            _ => None,
        }
    }

    fn get_expression_statement(statement: &Statement) -> Option<&ExpressionStatement> {
        match statement {
            Statement::ExpressionStatement(expression) => Some(expression),
            _ => None,
        }
    }

    fn check_return_statement(stmt: &Statement) {
        match stmt {
            ast::Statement::ReturnStatement(_ret_stmt) => {}
            _ => panic!("not return statement"),
        }
    }

    fn get_prefix_expression(expression: &Expression) -> Option<&PrefixExpression> {
        match expression {
            Expression::Prefix(prefix) => Some(prefix),
            _ => None,
        }
    }

    fn get_infix_expression(expression: &Expression) -> Option<&InfixExpression> {
        match expression {
            Expression::Infix(infix) => Some(infix),
            _ => None,
        }
    }

    fn check_number_expression(expression: &Expression, value: i32) {
        match expression {
            Expression::Number(num) => {
                assert_eq!(num.value, value);
            }
            _ => panic!("expected number expression, but {:?}", expression),
        }
    }

    fn check_identifier_expression(expression: &Expression, name: &str) {
        match expression {
            Expression::Identifier(iden) => {
                assert_eq!(&iden.name, name);
            }
            _ => panic!("expected identifier expression, but {:?}", expression),
        }
    }

    fn check_bool_expression(expression: &Expression, value: bool) {
        match expression {
            Expression::Bool(boolean) => {
                assert_eq!(boolean.value, value);
            }
            _ => panic!("expected boolean expression, but {:?}", expression),
        }
    }

    fn input_to_statements(input: &str) -> Vec<Statement> {
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        program.take_statement()
    }
}
