#![allow(clippy::new_without_default)]

use std::fmt::Display;

use crate::token::{IdentToken, Token};

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
    pub ident: IdentToken,
    pub expr: Expr,
}

impl LetStatement {
    pub fn new(ident: IdentToken, expr: Expr) -> Self {
        Self { ident, expr }
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
        write!(f, "let {} = {};", self.ident, self.expr)
    }
}

// pub struct IfStatement {}

#[derive(Debug)]
pub struct ReturnStatement {
    pub expression: Expr,
}

impl ReturnStatement {
    pub fn new(expression: Expr) -> Self {
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
    pub expression: Expr,
}

impl ExpressionStatement {
    pub fn new(expression: Expr) -> Self {
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

// #[derive(Debug)]
// pub struct Identifier {
//     pub name: String,
// }

// impl Identifier {
//     pub fn new(name: String) -> Self {
//         Self { name }
//     }
// }

// impl Display for Identifier {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}", self.name)
//     }
// }

#[derive(Debug)]
pub enum Expr {
    Identifier(IdentExpr),
    Number(NumberExpression),
    Bool(BoolExpression),
    Prefix(PrefixExpression),
    Infix(InfixExpr),
    If(IfExpression),
    Function(FunctionExpression),
    Call(CallExpression),
}

impl Expr {
    /// single expression - ideitifier나 int 같은 그 자체로 expression이 될수 있는 것들
    pub fn new_single_expression(token: Token) -> Option<Expr> {
        match token {
            Token::Ident(ident_tok) => Some(Expr::Identifier(IdentExpr { ident: ident_tok })),
            Token::Int(value) => Some(Expr::Number(NumberExpression { value })),
            Token::True => Some(Expr::Bool(BoolExpression { value: true })),
            Token::False => Some(Expr::Bool(BoolExpression { value: false })),
            _ => None,
        }
    }

    pub fn new_prefix_expression(op: Token, exp: Expr) -> Expr {
        Expr::Prefix(PrefixExpression {
            op,
            exp: Box::new(exp),
        })
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Identifier(identifier) => write!(f, "{}", identifier),
            Expr::Number(number) => write!(f, "{}", number),
            Expr::Bool(boolean) => write!(f, "{}", boolean),
            Expr::Prefix(prefix) => {
                write!(f, "{}{}", prefix.op, prefix.exp)
            }
            Expr::Infix(infix) => write!(f, "({} {} {})", infix.left, infix.op, infix.right),
            Expr::If(if_expression) => write!(f, "{}", if_expression),
            Expr::Function(fn_expression) => write!(f, "{}", fn_expression),
            Expr::Call(call_expression) => write!(f, "{}", call_expression),
        }
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub op: Token,
    pub exp: Box<Expr>,
}

#[derive(Debug)]
pub struct IdentExpr {
    pub ident: IdentToken,
}

impl Display for IdentExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
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
pub struct InfixExpr {
    pub left: Box<Expr>,
    pub op: Token,
    pub right: Box<Expr>,
}

impl InfixExpr {
    pub fn new(left: Expr, op: Token, right: Expr) -> Self {
        Self {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }
}

#[derive(Debug)]
pub struct IfExpression {
    pub condition: Box<Expr>,
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
    pub name: IdentToken,
    pub args: Vec<IdentToken>,
    pub block_statement: BlockStatement,
}

impl Display for FunctionExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}(", self.name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }

            write!(f, "{}", arg)?;
        }
        write!(f, ")")?;
        self.block_statement.fmt(f)
        // write!(f, "{}", self.block_statement)
    }
}

#[derive(Debug)]
pub struct CallExpression {
    pub fn_name: IdentToken,
    pub args: Vec<Expr>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.fn_name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }

            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}
