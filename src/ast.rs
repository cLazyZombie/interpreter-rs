#![allow(clippy::new_without_default)]

use std::fmt::Display;

use crate::token::Token;

pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
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

    pub fn take_statement(self) -> Vec<Statement> {
        self.statements
    }
}

#[derive(Debug)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStatement(let_stmt) => write!(f, "{}", let_stmt.to_string()),
            Statement::ReturnStatement(return_stmt) => write!(f, "{}", return_stmt.to_string()),
            Statement::ExpressionStatement(expression_stmt) => {
                write!(f, "{}", expression_stmt.to_string())
            }
            Statement::BlockStatement(block_stmt) => write!(f, "{}", block_stmt.to_string()),
        }
    }
}
#[derive(Debug)]
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
    // pub fn to_string(&self) -> String {
    //     format!(
    //         "let {} = {};",
    //         self.identifier.name,
    //         self.expression.to_string()
    //     )
    // }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {};", self.identifier, self.expression)
    }
}

// pub struct IfStatement {}

#[derive(Debug)]
pub struct ReturnStatement {
    pub expression: Expression,
}

impl ReturnStatement {
    pub fn new(expression: Expression) -> Self {
        Self { expression }
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {};", self.expression)
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

impl ExpressionStatement {
    pub fn new(expression: Expression) -> Self {
        Self { expression }
    }
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{};", self.expression)
    }
}

#[derive(Debug)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        for stmt in &self.statements {
            writeln!(f, "{}", stmt)?;
        }
        writeln!(f, "}}")
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub name: String,
}

impl Identifier {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier(IdentifierExpression),
    Number(NumberExpression),
    Bool(BoolExpression),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfExpression),
    Function(FunctionExpression),
}

impl Expression {
    /// single expression - ideitifier나 int 같은 그 자체로 expression이 될수 있는 것들
    pub fn new_single_expression(token: Token) -> Option<Expression> {
        match token {
            Token::Iden(name) => Some(Expression::Identifier(IdentifierExpression { name })),
            Token::Int(value) => Some(Expression::Number(NumberExpression { value })),
            Token::True => Some(Expression::Bool(BoolExpression { value: true })),
            Token::False => Some(Expression::Bool(BoolExpression { value: false })),
            _ => None,
        }
    }

    pub fn new_infix_expression(left: Expression, op: Token, right: Expression) -> Expression {
        Expression::Infix(InfixExpression {
            left: Box::new(left),
            op,
            right: Box::new(right),
        })
    }

    pub fn new_prefix_expression(op: Token, exp: Expression) -> Expression {
        Expression::Prefix(PrefixExpression {
            op,
            exp: Box::new(exp),
        })
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(identifier) => write!(f, "{}", identifier),
            Expression::Number(number) => write!(f, "{}", number),
            Expression::Bool(boolean) => write!(f, "{}", boolean),
            Expression::Prefix(prefix) => {
                write!(f, "{}{}", prefix.op, prefix.exp)
            }
            Expression::Infix(infix) => write!(f, "({} {} {})", infix.left, infix.op, infix.right),
            Expression::If(if_expression) => write!(f, "{}", if_expression),
            Expression::Function(fn_expression) => write!(f, "{}", fn_expression),
        }
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub op: Token,
    pub exp: Box<Expression>,
}

#[derive(Debug)]
pub struct IdentifierExpression {
    pub name: String,
}

impl Display for IdentifierExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug)]
pub struct NumberExpression {
    pub value: i32,
}

impl Display for NumberExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug)]
pub struct BoolExpression {
    pub value: bool,
}

impl Display for BoolExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub op: Token,
    pub right: Box<Expression>,
}

#[derive(Debug)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence_statement: BlockStatement,
    pub alternative_statement: Option<BlockStatement>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if ({}) {}", self.condition, self.consequence_statement)?;
        if let Some(alternative) = &self.alternative_statement {
            write!(f, "else {}", alternative)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct FunctionExpression {
    pub name: Identifier,
    pub args: Vec<Identifier>,
    pub block_statement: BlockStatement,
}

impl Display for FunctionExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}(", self.name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }

            write!(f, "{}", arg.name)?;
        }
        write!(f, ")")?;
        self.block_statement.fmt(f)
        // write!(f, "{}", self.block_statement)
    }
}
