#[derive(PartialEq, Eq, Debug)]
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
