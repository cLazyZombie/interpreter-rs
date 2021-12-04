use std::io::{self, BufRead, Write};

use interpreter_rs::{lexer::Lexer, token::Token};

fn main() {
    print!(">> ");
    let _ = io::stdout().lock().flush();

    for line in io::stdin().lock().lines() {
        let line = line.unwrap();
        let mut lexer = Lexer::new(&line);

        while let Some(tok) = lexer.next_token() {
            println!("{:?}", tok);
            if tok == Token::EOF {
                break;
            }
        }
        print!(">> ");
        let _ = io::stdout().lock().flush();
    }
}
