use std::fmt::Display;

use crate::parser::Precedence;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    Illegal,
    EOF,
    Iden(String),
    Int(i32),
    Assign,
    Plus,
    Minus,
    Bang,      // !
    Asterrisk, // *
    Slash,     // /
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace, // {
    RBrace,
    Eq,    // ==
    NotEq, // !=
    LT,    // <
    GT,    // >
    // 예약어
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Token {
    pub fn is_infix_op(&self) -> bool {
        matches!(
            self,
            Token::Plus
                | Token::Minus
                | Token::Asterrisk
                | Token::Slash
                | Token::Eq
                | Token::NotEq
                | Token::LT
                | Token::GT
        )
    }

    pub fn is_prefix_op(&self) -> bool {
        matches!(self, Token::Minus | Token::Bang)
    }

    pub fn precedence(&self) -> Option<Precedence> {
        match self {
            Token::Eq | Token::NotEq => Some(Precedence::Equals),
            Token::LT | Token::GT => Some(Precedence::LessGreater),
            Token::Plus | Token::Minus => Some(Precedence::Sum),
            Token::Asterrisk | Token::Slash => Some(Precedence::Product),
            _ => None,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Illegal => write!(f, "illegal"),
            Token::EOF => write!(f, "eof"),
            Token::Iden(s) => write!(f, "{}", s),
            Token::Int(i) => write!(f, "{}", i),
            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Bang => write!(f, "!"),
            Token::Asterrisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Eq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::LT => write!(f, "<"),
            Token::GT => write!(f, ">"),
            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
        }
    }
}
