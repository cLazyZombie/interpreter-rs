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
}

pub struct LetStatement {
    pub identifier: Identifier,
    pub value: Expression,
}

impl LetStatement {
    pub fn new(identifier: Identifier, value: Expression) -> Self {
        Self { identifier, value }
    }
}

pub struct IfStatement {}

pub struct ReturnStatement {
    pub value: Expression,
}

impl ReturnStatement {
    pub fn new(value: Expression) -> Self {
        Self { value }
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
