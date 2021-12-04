use std::{iter::Peekable, str::Chars};

use crate::token::Token;

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().peekable(),
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        let next_char;

        // skip white space
        loop {
            match self.take_next_char() {
                None => return Some(Token::EOF),
                Some(ch) => {
                    if !ch.is_whitespace() {
                        next_char = ch;
                        break;
                    }
                }
            }
        }

        let token = match next_char {
            '=' => Token::ASSIGN,
            ';' => Token::SEMICOLON,
            '(' => Token::LPAREN,
            ')' => Token::RPAREN,
            ',' => Token::COMMA,
            '+' => Token::PLUS,
            '{' => Token::LBRACE,
            '}' => Token::RBRACE,
            _ => {
                if next_char.is_letter() {
                    let identi = self.read_identifier(next_char);
                    match &identi as &str {
                        "let" => Token::LET,
                        _ => Token::IDENT(identi),
                    }
                } else {
                    if let Some(num) = self.read_int(next_char) {
                        Token::INT(num)
                    } else {
                        Token::ILLEGAL
                    }
                }
            }
        };

        Some(token)
    }

    fn peek_next_char(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn take_next_char(&mut self) -> Option<char> {
        self.chars.next().clone()
    }

    fn read_identifier(&mut self, init: char) -> String {
        let mut s = String::new();
        s.push(init);

        while self.peek_next_char().is_letter() {
            s.push(self.take_next_char().unwrap());
        }

        s
    }

    fn read_int(&mut self, init: char) -> Option<i32> {
        let mut s = String::new();
        s.push(init);

        while let Some(next) = self.peek_next_char() {
            if !next.is_numeric() {
                break;
            }

            s.push(self.take_next_char().unwrap());
        }

        if let Ok(num) = s.parse::<i32>() {
            Some(num)
        } else {
            None
        }
    }
}

trait IsLetter {
    fn is_letter(&self) -> bool;
}

impl IsLetter for Option<&char> {
    fn is_letter(&self) -> bool {
        if let Some(ch) = *self {
            ch.is_letter()
        } else {
            false
        }
    }
}

impl IsLetter for char {
    fn is_letter(&self) -> bool {
        match *self {
            'a'..='z' => true,
            'A'..='Z' => true,
            '_' => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::Token;

    use super::*;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";
        let mut lexer = Lexer::new(input);

        let expected = [
            Some(Token::ASSIGN),
            Some(Token::PLUS),
            Some(Token::LPAREN),
            Some(Token::RPAREN),
            Some(Token::LBRACE),
            Some(Token::RBRACE),
            Some(Token::COMMA),
            Some(Token::SEMICOLON),
            Some(Token::EOF),
        ];

        for exp in expected {
            assert_eq!(lexer.next_token(), exp);
        }
    }

    #[test]
    fn multi_line() {
        let input = r#"
            let val_a = 10;
            let val_b = 20;

        "#;

        let mut lexer = Lexer::new(input);
        let expected_tokens = [
            Some(Token::LET),
            Some(Token::IDENT("val_a".to_string())),
            Some(Token::ASSIGN),
            Some(Token::INT(10)),
            Some(Token::SEMICOLON),
            Some(Token::LET),
            Some(Token::IDENT("val_b".to_string())),
            Some(Token::ASSIGN),
            Some(Token::INT(20)),
            Some(Token::SEMICOLON),
            Some(Token::EOF),
        ];

        for tok in expected_tokens {
            assert_eq!(lexer.next_token(), tok);
        }
    }

    #[test]
    fn test_let_binding() {
        let input = "let five = 5;";
        let mut lexer = Lexer::new(input);

        let expected_tokens = [
            Some(Token::LET),
            Some(Token::IDENT("five".to_string())),
            Some(Token::ASSIGN),
            Some(Token::INT(5)),
            Some(Token::SEMICOLON),
            Some(Token::EOF),
        ];

        for tok in expected_tokens {
            assert_eq!(lexer.next_token(), tok);
        }
    }
}
