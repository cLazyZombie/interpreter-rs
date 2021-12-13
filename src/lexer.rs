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
            '=' => {
                if self.peek_next_char() == Some(&'=') {
                    self.take_next_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            '!' => {
                if self.peek_next_char() == Some(&'=') {
                    self.take_next_char();
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Asterrisk,
            '/' => Token::Slash,
            '<' => Token::LT,
            '>' => Token::GT,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            _ => {
                if next_char.is_letter() {
                    let identi = self.read_identifier(next_char);
                    match &identi as &str {
                        "let" => Token::Let,
                        "fn" => Token::Function,
                        "true" => Token::True,
                        "false" => Token::False,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "return" => Token::Return,
                        _ => Token::Iden(identi),
                    }
                } else if let Some(num) = self.read_int(next_char) {
                    Token::Int(num)
                } else {
                    eprintln!("illegal {}", next_char);
                    Token::Illegal
                }
            }
        };

        Some(token)
    }

    fn peek_next_char(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn take_next_char(&mut self) -> Option<char> {
        self.chars.next()
    }

    fn read_identifier(&mut self, init: char) -> String {
        let mut s = String::new();
        s.push(init);

        while self.peek_next_char().is_letter() || self.peek_next_char().is_num() {
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

trait CharTypeChecker {
    fn is_letter(&self) -> bool;
    fn is_num(&self) -> bool;
}

impl CharTypeChecker for Option<&char> {
    fn is_letter(&self) -> bool {
        if let Some(ch) = *self {
            ch.is_letter()
        } else {
            false
        }
    }

    fn is_num(&self) -> bool {
        if let Some(ch) = *self {
            ch.is_num()
        } else {
            false
        }
    }
}

impl CharTypeChecker for char {
    fn is_letter(&self) -> bool {
        matches!(*self, 'a'..='z' | 'A'..='Z' | '_')
    }

    fn is_num(&self) -> bool {
        matches!(*self, '0'..='9')
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
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
            Token::EOF,
        ];

        for exp in expected {
            assert_eq!(lexer.next_token(), Some(exp));
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
            Token::Let,
            Token::Iden("val_a".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Iden("val_b".to_string()),
            Token::Assign,
            Token::Int(20),
            Token::Semicolon,
            Token::EOF,
        ];

        for tok in expected_tokens {
            assert_eq!(lexer.next_token(), Some(tok));
        }
    }

    #[test]
    fn test_let_binding() {
        let input = "let five = 5;";
        let mut lexer = Lexer::new(input);

        let expected_tokens = [
            Token::Let,
            Token::Iden("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::EOF,
        ];

        for tok in expected_tokens {
            assert_eq!(lexer.next_token(), Some(tok));
        }
    }

    #[test]
    fn tokenize_fn() {
        let input = r#"
            let a = 5;
            let add = fn(x, y) {
                x + y;
            };
        "#;
        let mut lexer = Lexer::new(input);

        let expected_tokens = [
            Token::Let,
            Token::Iden("a".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Iden("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Iden("x".to_string()),
            Token::Comma,
            Token::Iden("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Iden("x".to_string()),
            Token::Plus,
            Token::Iden("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::EOF,
        ];

        for tok in expected_tokens {
            assert_eq!(lexer.next_token(), Some(tok));
        }
    }

    #[test]
    fn equal_not_equal() {
        let input = r#"
            a == b
            a != b
        "#;
        let mut lexer = Lexer::new(input);

        let expected_tokens = [
            Token::Iden("a".to_string()),
            Token::Eq,
            Token::Iden("b".to_string()),
            Token::Iden("a".to_string()),
            Token::NotEq,
            Token::Iden("b".to_string()),
        ];

        for tok in expected_tokens {
            assert_eq!(lexer.next_token(), Some(tok));
        }
    }

    #[test]
    fn function_name() {
        let input = "fn my_func_1() {}";
        let mut lexer = Lexer::new(input);
        let expected_tokens = [
            Token::Function,
            Token::Iden("my_func_1".to_string()),
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
        ];

        for tok in expected_tokens {
            assert_eq!(lexer.next_token(), Some(tok));
        }
    }
}
