use crate::{ast, lexer::Lexer, token::Token};

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

        let expression = self.parse_expression()?;

        let let_statement = ast::LetStatement::new(identifier, expression);
        Ok(let_statement)
    }

    fn parse_return_statement(&mut self) -> Result<ast::ReturnStatement, ParseError> {
        self.advancd_token(); // return

        let expression = self.parse_expression()?;
        let return_stmt = ast::ReturnStatement::new(expression);
        Ok(return_stmt)
    }

    fn parse_expression_statement(&mut self) -> Result<ast::ExpressionStatement, ParseError> {
        let expression = self.parse_expression()?;
        let expression_stmt = ast::ExpressionStatement::new(expression);
        Ok(expression_stmt)
    }

    fn parse_identifier(&mut self) -> Result<ast::Identifier, ParseError> {
        let id_token = self.cur_token();
        if id_token.is_none() {
            return Err(ParseError::NoMoreToken(
                "identifier token is missing".to_string(),
            ));
        }

        let id_token = id_token.unwrap();
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

    fn parse_expression(&mut self) -> Result<ast::Expression, ParseError> {
        let ret = match self.cur_token() {
            Some(Token::Int(v)) => {
                let exp = ast::Expression::new(v.to_string());
                self.advancd_token();

                Ok(exp)
            }
            Some(Token::Iden(iden)) => {
                let exp = ast::Expression::new(iden);
                self.advancd_token();

                Ok(exp)
            }
            tok => {
                let err = format!("Token::Int is expected, but {:?}", tok);
                Err(ParseError::UnexpectedToken(err))
            }
        };

        if let Ok(_) = ret {
            if matches!(self.cur_token(), Some(Token::Semicolon)) {
                self.advancd_token();
            }
        }

        ret
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Statement;

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
    }
}
