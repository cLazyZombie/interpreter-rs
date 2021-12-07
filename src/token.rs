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
    pub fn is_infix(&self) -> bool {
        match self {
            Token::Plus
            | Token::Minus
            | Token::Asterrisk
            | Token::Slash
            | Token::Eq
            | Token::NotEq
            | Token::LT
            | Token::GT => true,
            _ => false,
        }
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

    pub fn to_string(&self) -> String {
        match self {
            Token::Illegal => "illegal".to_string(),
            Token::EOF => "eof".to_string(),
            Token::Iden(s) => s.clone(),
            Token::Int(i) => i.to_string(),
            Token::Assign => "=".to_string(),
            Token::Plus => "+".to_string(),
            Token::Minus => "-".to_string(),
            Token::Bang => "!".to_string(),      // !
            Token::Asterrisk => "*".to_string(), // *
            Token::Slash => "/".to_string(),     // /
            Token::Comma => ",".to_string(),
            Token::Semicolon => ";".to_string(),
            Token::LParen => "(".to_string(),
            Token::RParen => ")".to_string(),
            Token::LBrace => "{".to_string(), // {
            Token::RBrace => "}".to_string(),
            Token::Eq => "==".to_string(),    // ==
            Token::NotEq => "!=".to_string(), // !=
            Token::LT => "<".to_string(),     // <
            Token::GT => ">".to_string(),     // >
            Token::Function => "fn".to_string(),
            Token::Let => "let".to_string(),
            Token::True => "true".to_string(),
            Token::False => "false".to_string(),
            Token::If => "if".to_string(),
            Token::Else => "else".to_string(),
            Token::Return => "return".to_string(),
        }
    }
}
