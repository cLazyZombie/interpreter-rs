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

        eprintln!("before parsing");

        while let Some(token) = lexer.next_token() {
            if matches!(token, Token::EOF) {
                break;
            }
            tokens.push(token);
        }

        eprintln!("start parsing");

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
        let mut program = ast::Program::new();

        while let Some(token) = self.cur_token() {
            self.advancd_token();

            match token {
                Token::Let => {
                    eprintln!("a");
                    let stmt = ast::Statement::LetStatement(self.parse_let_statement()?);
                    eprintln!("b");
                    program.add(stmt);
                }
                _ => {
                    dbg!(token);
                    todo!()
                }
            }
        }

        Ok(program)
    }

    fn parse_let_statement(&mut self) -> Result<ast::LetStatement, ParseError> {
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
        // tood. impl
        match self.cur_token() {
            Some(Token::Int(v)) => {
                assert!(matches!(self.next_token(), Some(Token::Semicolon)));
                let exp = ast::Expression::new(v);

                self.advancd_token();
                self.advancd_token();

                Ok(exp)
            }
            tok => {
                let err = format!("Token::Int is expected, but {:?}", tok);
                Err(ParseError::UnexpectedToken(err))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn let_statement() {
        let input = "let val = 10;";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        assert_eq!(program.statement_count(), 1);

        let statement = program.get_statement(0).unwrap();
        match statement {
            ast::Statement::LetStatement(let_statement) => {
                assert_eq!(let_statement.identifier.name, "val".to_string());
                assert_eq!(let_statement.value.value, 10);
            }
        }
    }
}
