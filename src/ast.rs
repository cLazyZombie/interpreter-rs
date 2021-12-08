#![allow(clippy::new_without_default)]

use crate::token::Token;

pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }

    pub fn add(&mut self, stmt: Statement) {
        self.statements.push(stmt);
    }

    pub fn statement_count(&self) -> usize {
        self.statements.len()
    }

    pub fn get_statement(&self, idx: usize) -> Option<&Statement> {
        self.statements.get(idx)
    }
}

pub enum Statement {
    LetStatement(LetStatement),
    // IfStatement(IfStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl Statement {
    pub fn to_string(&self) -> String {
        match self {
            Statement::LetStatement(let_stmt) => let_stmt.to_string(),
            Statement::ReturnStatement(return_stmt) => return_stmt.to_string(),
            Statement::ExpressionStatement(expression_stmt) => expression_stmt.to_string(),
        }
    }
}

pub struct LetStatement {
    pub identifier: Identifier,
    pub expression: Expression,
}

impl LetStatement {
    pub fn new(identifier: Identifier, expression: Expression) -> Self {
        Self {
            identifier,
            expression,
        }
    }

    pub fn to_string(&self) -> String {
        format!(
            "let {} = {};",
            self.identifier.name,
            self.expression.to_string()
        )
    }
}

// pub struct IfStatement {}

pub struct ReturnStatement {
    pub expression: Expression,
}

impl ReturnStatement {
    pub fn new(expression: Expression) -> Self {
        Self { expression }
    }

    pub fn to_string(&self) -> String {
        format!("return {};", self.expression.to_string())
    }
}

pub struct ExpressionStatement {
    pub expression: Expression,
}

impl ExpressionStatement {
    pub fn new(expression: Expression) -> Self {
        Self { expression }
    }

    pub fn to_string(&self) -> String {
        format!("{};", self.expression.to_string())
    }
}

pub struct Identifier {
    pub name: String,
}

impl Identifier {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier(IdentifierExpression),
    Number(NumberExpression),
    Infix(InfixExpression),
}

impl Expression {
    pub fn prefix(token: Token) -> Option<Expression> {
        match token {
            Token::Iden(name) => Some(Expression::Identifier(IdentifierExpression { name })),
            Token::Int(value) => Some(Expression::Number(NumberExpression { value })),
            _ => None,
        }
    }

    pub fn infix(left: Expression, op: Token, right: Expression) -> Expression {
        Expression::Infix(InfixExpression {
            left: Box::new(left),
            op,
            right: Box::new(right),
        })
    }

    pub fn to_string(&self) -> String {
        match self {
            Expression::Identifier(identifier) => identifier.name.clone(),
            Expression::Number(number) => number.value.to_string(),
            Expression::Infix(infix) => format!(
                "{} {} {}",
                infix.left.to_string(),
                infix.op.to_string(),
                infix.right.to_string()
            ),
        }
    }
}

#[derive(Debug)]
pub struct IdentifierExpression {
    pub name: String,
}

#[derive(Debug)]
pub struct NumberExpression {
    pub value: i32,
}

#[derive(Debug)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub op: Token,
    pub right: Box<Expression>,
}

// pub struct Expression {
//     pub value: String,
// }

// impl Expression {
//     pub fn new(value: String) -> Self {
//         Self { value }
//     }
// }
