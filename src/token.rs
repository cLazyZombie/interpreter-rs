#[derive(PartialEq, Eq, Debug)]
pub enum Token {
    ILLEGAL,
    EOF,
    IDENT(String),
    INT(i32),
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE, // {
    RBRACE,
    // 예약어
    FUNCTION,
    LET,
}
