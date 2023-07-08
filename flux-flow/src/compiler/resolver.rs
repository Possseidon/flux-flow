use std::collections::BTreeMap;

use thiserror::Error;

use super::{diagnostic::ResultWithDiagnostics, parser::syntax_tree::SyntaxTree};

#[derive(Debug)]
pub struct Ast {
    modules: Vec<Module>,
    functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Module {
    functions: BTreeMap<String, Function>,
}

#[derive(Debug)]
pub struct Function {
    argument: AssigneeExpression,
    // result_type: StaticType,
    statements: Vec<Statement>,
    last_expression: Option<Expression>,
}

#[derive(Debug)]
pub enum AssigneeExpression {
    Discard,
}

#[derive(Debug)]
pub struct Statement;

#[derive(Debug)]
pub struct Expression;

pub type ResolveResult<'code> = ResultWithDiagnostics<'code, Ast>;

#[derive(Debug, Error)]
pub enum ResolveError {}

pub fn resolve(code: &str, syntax_tree: SyntaxTree) -> ResolveResult {
    let root_module = syntax_tree.root_module();
    ResolveResult::unknown_error(code)
}
