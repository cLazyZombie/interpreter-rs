use crate::{
    ast::{self, PrefixExpression},
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
        let mut program = ast::Program::new();

        while let Some(token) = self.cur_token() {
            match token {
                Token::Let => {
                    let stmt = ast::Statement::LetStatement(self.parse_let_statement()?);
                    program.add(stmt);
                }
                Token::Return => {
                    let stmt = ast::Statement::ReturnStatement(self.parse_return_statement()?);
                    program.add(stmt);
                }
                _ => {
                    let stmt =
                        ast::Statement::ExpressionStatement(self.parse_expression_statement()?);
                    program.add(stmt);
                }
            }
        }

        Ok(program)
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
            let prefix = ast::Expression::Prefix(PrefixExpression {
                op: tok,
                exp: Box::new(exp),
            });

            prefix
        } else {
            let result = ast::Expression::prefix(tok.clone()).ok_or_else(|| {
                let err = format!("expression token expected, but {:?}", tok);
                ParseError::UnexpectedToken(err)
            })?;

            self.advancd_token();
            result
        };

        Ok(exp)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<ast::Expression, ParseError> {
        let mut result = self.parse_prefix_expression()?;

        // exit expression parsing if semicolon encountered
        if self.cur_token() == Some(Token::Semicolon) {
            self.advancd_token();
            return Ok(result);
        }

        while let Some(cur_token) = self.cur_token() {
            if cur_token.is_infix_op() {
                let cur_precedence = cur_token.precedence().unwrap(); // infix token should have precedencd
                if cur_precedence > precedence {
                    self.advancd_token();

                    let right_expression = self.parse_expression(cur_precedence)?;
                    let infix_expression =
                        ast::Expression::infix(result, cur_token, right_expression);
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
        Expression, ExpressionStatement, InfixExpression, PrefixExpression, Statement,
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

    fn check_let_statement(stmt: &Statement, name: &str) {
        match stmt {
            ast::Statement::LetStatement(let_stmt) => {
                assert_eq!(let_stmt.identifier.name, name.to_string());
            }
            _ => panic!("not let statement"),
        }
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

    fn check_return_statement(stmt: &Statement) {
        match stmt {
            ast::Statement::ReturnStatement(_ret_stmt) => {}
            _ => panic!("not return statement"),
        }
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

    fn check_expression_statement(stmt: &Statement) {
        match stmt {
            ast::Statement::ExpressionStatement(_expr) => {}
            _ => panic!("not expression statement"),
        }
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
        let input = "-1;";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let expression_statement =
            get_expression_statement(program.get_statement(0).unwrap()).unwrap();
        let prefix_expression = get_prefix_expression(&expression_statement.expression).unwrap();
        assert_eq!(prefix_expression.op, Token::Minus);
        check_number_expression(&prefix_expression.exp, 1);
    }

    fn get_expression_statement(statement: &Statement) -> Option<&ExpressionStatement> {
        match statement {
            Statement::ExpressionStatement(expression) => Some(expression),
            _ => None,
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
}
