use std::collections::BTreeMap;

use thiserror::Error;

use crate::value::Value;

use super::{diagnostic::ResultWithDiagnostics, resolver};

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Module<'code> {
    code: &'code str,
    values: BTreeMap<String, Value>,
}

pub type AssembleResult<'code> = ResultWithDiagnostics<'code, Module<'code>>;

#[derive(Debug, Error)]
pub enum AssembleError {}

pub fn assemble(code: &str, ast: resolver::Ast) -> AssembleResult {
    AssembleResult::unknown_error(code)
}
