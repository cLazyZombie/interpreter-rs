use std::io::{self, BufRead, Write};

use interpreter_rs::{eval::eval, lexer::Lexer, parser::Parser};

fn main() {
    print!(">> ");
    let _ = io::stdout().lock().flush();

    for line in io::stdin().lock().lines() {
        let line = line.unwrap();
        let lexer = Lexer::new(&line);
        let mut parser = Parser::new(lexer);
        let stmts = parser.parse_statements();
        match stmts {
            Ok(stmts) => {
                for stmt in stmts {
                    let object = eval(&stmt);
                    match object {
                        Ok(object) => {
                            println!("{}", object);
                        }
                        Err(err) => {
                            println!("Error. {:?}", err);
                        }
                    }
                }
            }
            Err(err) => {
                println!("Error. {:?}", err);
            }
        }

        print!(">> ");
        let _ = io::stdout().lock().flush();
    }
}
