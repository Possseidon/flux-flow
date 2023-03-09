
pub struct Block {
    uses: BTreeMap<Identifier, Path>,
    statements: Vec<Statement>,
    last_expression: Option<Expression>,
}
