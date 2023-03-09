pub mod assembler;
pub mod diagnostic;
pub mod lexer;
pub mod parser;
pub mod resolver;

pub fn compile(code: &str) -> assembler::AssembleResult {
    let parse_result = parser::parse(code);

    let mut resolve_result = resolver::resolve(code, parse_result.syntax_tree);
    resolve_result.diagnostics.extend(parse_result.diagnostics);
    let Some(ast) = resolve_result.value else {
        return assembler::AssembleResult::error(code, resolve_result.diagnostics);
    };

    let mut assemble_result = assembler::assemble(code, ast);
    assemble_result
        .diagnostics
        .extend(resolve_result.diagnostics);
    assemble_result
}
