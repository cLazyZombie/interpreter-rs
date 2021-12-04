#[derive(PartialEq, Eq, Debug)]
pub enum Token {
    Illegal,
    EOF,
    Iden(String),
    Int(i32),
    Assign,
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace, // {
    RBrace,
    Eq,    // ==
    NotEq, // !=
    // 예약어
    Function,
    Let,
}
