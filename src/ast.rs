#![allow(clippy::new_without_default)]

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
    IfStatement(IfStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
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
}

pub struct IfStatement {}

pub struct ReturnStatement {
    pub expression: Expression,
}

impl ReturnStatement {
    pub fn new(expression: Expression) -> Self {
        Self { expression }
    }
}

pub struct ExpressionStatement {
    pub expression: Expression,
}

impl ExpressionStatement {
    pub fn new(expression: Expression) -> Self {
        Self { expression }
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

pub struct Expression {
    pub value: i32,
}

impl Expression {
    pub fn new(value: i32) -> Self {
        Self { value }
    }
}
