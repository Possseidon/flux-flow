use lazy_static::lazy_static;

use crate::compiler::lexer::{BraceKind, TokenKind};

use super::{
    grammar::{Grammar, GrammarBuilder, OptionalRule, RecursiveRule, Rule},
    syntax_tree_node_builder::{Buildable, NodeBuilderError, NodeBuilderReader},
};

#[derive(Debug)]
pub struct SyntaxTree {
    initial_whitespace: usize,
    nodes: SyntaxTreeNodes,
}

#[derive(Debug, Default)]
struct SyntaxTreeNodes {
    argument_expressions: Vec<ArgumentExpression>,
    array_literals: Vec<ArrayLiteral>,
    binary_operations: Vec<BinaryOperation>,
    binary_operators: Vec<BinaryOperator>,
    blocks: Vec<Block>,
    break_expressions: Vec<BreakExpression>,
    conditions: Vec<Condition>,
    continue_expressions: Vec<ContinueExpression>,
    else_blocks: Vec<ElseBlock>,
    else_if_blocks: Vec<ElseIfBlock>,
    else_if_chains: Vec<ElseIfChain>,
    expressions: Vec<Expression>,
    for_blocks: Vec<ForBlock>,
    functions: Vec<Function>,
    group_expressions: Vec<GroupExpression>,
    if_blocks: Vec<IfBlock>,
    items: Vec<Item>,
    labeled_blocks: Vec<LabeledBlock>,
    let_bindings: Vec<LetBinding>,
    loop_blocks: Vec<LoopBlock>,
    map_entries: Vec<MapEntry>,
    map_entry_chains: Vec<MapEntryChain>,
    map_literals: Vec<MapLiteral>,
    match_arms: Vec<MatchArm>,
    match_blocks: Vec<MatchBlock>,
    match_bodies: Vec<MatchBody>,
    modules: Vec<Module>,
    path_resolution_chains: Vec<PathResolutionChain>,
    path_resolutions: Vec<PathResolution>,
    patterns: Vec<Pattern>,
    prefix_operation_chains: Vec<PrefixOperationChain>,
    prefix_operations: Vec<PrefixOperation>,
    return_expressions: Vec<ReturnExpression>,
    return_types: Vec<ReturnType>,
    root_expressions: Vec<RootExpression>,
    set_item_chains: Vec<SetItemChain>,
    set_literals: Vec<SetLiteral>,
    statements: Vec<Statement>,
    struct_field_chains: Vec<StructFieldChain>,
    struct_field_names: Vec<StructFieldName>,
    struct_fields: Vec<StructField>,
    struct_literals: Vec<StructLiteral>,
    suffix_operation_chains: Vec<SuffixOperationChain>,
    suffix_operations: Vec<SuffixOperation>,
    trailing_comma_expressions: Vec<TrailingCommaExpression>,
    tuple_field_chains: Vec<TupleFieldChain>,
    tuple_fields: Vec<TupleField>,
    tuple_indices: Vec<TupleIndex>,
    tuple_literals: Vec<TupleLiteral>,
    type_expressions: Vec<TypeExpression>,
    types: Vec<Type>,
    unary_operators: Vec<UnaryOperator>,
    while_blocks: Vec<WhileBlock>,
}

impl SyntaxTree {
    pub fn new(initial_whitespace: usize) -> Self {
        Self {
            initial_whitespace,
            nodes: SyntaxTreeNodes::default(),
        }
    }

    pub fn initial_whitespace(&self) -> usize {
        self.initial_whitespace
    }

    pub fn root_module(&self) -> &Module {
        self.nodes.modules.last().expect("root module should exist")
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NodeRef(usize);

impl NodeRef {
    pub fn is_root(&self) -> bool {
        self.0 == 0
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Token {
    pub index: usize,
    pub len: usize,
    pub trailing_whitespace: usize,
}

macro_rules! rule {
    ( $grammar:ident $token_kind:ident ) => {
        Rule::Token(TokenKind::$token_kind)
    };
    ( $grammar:ident ( $nodes:ident: $T:ty ) ) => {
        Rule::Ref($grammar.rule::<$T>())
    };
}

macro_rules! optional_rule {
    ( $grammar:ident $token_kind:ident ) => {
        OptionalRule::Required(Rule::Token(TokenKind::$token_kind))
    };
    ( $grammar:ident [ $token_kind:ident ] ) => {
        OptionalRule::Optional(Rule::Token(TokenKind::$token_kind))
    };
    ( $grammar:ident ( $nodes:ident: $T:ty ) ) => {
        OptionalRule::Required(Rule::Ref($grammar.rule::<$T>()))
    };
    ( $grammar:ident [ $nodes:ident: $T:ty ] ) => {
        OptionalRule::Optional(Rule::Ref($grammar.rule::<$T>()))
    };
}

macro_rules! field_type {
    ( $rule:ident ) => {
        Token
    };
    ( [ $rule:ident ] ) => {
        Option<Token>
    };
    ( ( $nodes:ident: $T:ty ) ) => {
        NodeRef
    };
    ( [ $nodes:ident: $T:ty ] ) => {
        Option<NodeRef>
    };
}

macro_rules! ref_field_type {
    ( $rule:ident ) => {
        Token
    };
    ( ( $nodes:ident: $T:ty ) ) => {
        $T
    };
}

macro_rules! field_accessor {
    ( $field:ident $rule:ident ) => {
        pub fn $field(&self) -> Token {
            self.$field
        }
    };
    ( $field:ident [ $rule:ident ] ) => {
        pub fn $field(&self) -> Option<Token> {
            self.$field
        }
    };
    ( $field:ident ( $nodes:ident: $T:ty ) ) => {
        pub fn $field<'a>(&self, syntax_tree: &'a SyntaxTree) -> &'a $T {
            &syntax_tree.nodes.$nodes[self.$field.0]
        }
    };
    ( $field:ident [ $nodes:ident: $T:ty ] ) => {
        pub fn $field<'a>(&self, syntax_tree: &'a SyntaxTree) -> Option<&'a $T> {
            self.$field.map(|NodeRef(i)| &syntax_tree.nodes.$nodes[i])
        }
    };
}

macro_rules! concatenation {
    { $syntax_tree_name:ident: $T:ident {
        $( $essential_fields:ident: $EssentialFields:tt, )*
        > $last_essential_field:ident: $LastEssentialField:tt,
        $( $required_fields:ident: $RequiredFields:tt, )*
    } } => {
        #[derive(Debug)]
        pub struct $T {
            $( $essential_fields: field_type!($EssentialFields), )*
            $last_essential_field: field_type!($LastEssentialField),
            $( $required_fields: field_type!($RequiredFields), )*
        }

        impl $T {
            $( field_accessor!($essential_fields $EssentialFields); )*
            field_accessor!($last_essential_field $LastEssentialField);
            $( field_accessor!($required_fields $RequiredFields); )*

            pub fn grammar() -> &'static Grammar {
                lazy_static! {
                    static ref GRAMMAR: Grammar = Grammar::new::<$T>();
                }
                &GRAMMAR
            }
        }

        impl Buildable for $T {
            fn name() -> &'static str {
                stringify!($T)
            }

            fn rule(#[allow(unused_variables)] grammar: &mut GrammarBuilder) -> RecursiveRule {
                RecursiveRule::Concatenation {
                    essential: vec![ $( optional_rule!(grammar $EssentialFields), )* ],
                    last_essential: rule!(grammar $LastEssentialField),
                    required: vec![ $( optional_rule!(grammar $RequiredFields), )* ],
                }
            }

            fn build(
                syntax_tree: &mut SyntaxTree,
                reader: &mut NodeBuilderReader,
            ) -> Result<Option<NodeRef>, NodeBuilderError> {
                let node_ref = NodeRef(syntax_tree.nodes.$syntax_tree_name.len());
                syntax_tree.nodes.$syntax_tree_name.push(Self {
                    $( $essential_fields: reader.read()?, )*
                    $last_essential_field: reader.read()?,
                    $( $required_fields: reader.read()?, )*
                });
                Ok(Some(node_ref))
            }
        }
    };
}

macro_rules! alternation_ref {
    ( $Ref:ident $syntax_tree:ident $node:ident $alternative:ident ( $nodes:ident: $T:ty ) ) => {
        $Ref::$alternative(&$syntax_tree.nodes.$nodes[$node.0])
    };
    ( $Ref:ident $syntax_tree:ident $token:ident $alternative:ident $rule:ident ) => {
        $Ref::$alternative($token)
    };
}

macro_rules! alternation {
    { $syntax_tree_name:ident: $T:ident $Impl:ident $Ref:ident {
        $( $alternative:ident: $Alternative:tt, )*
    } } => {
        #[derive(Debug)]
        pub struct $T(pub $Impl);

        impl $T {
            pub fn get<'a>(&'a self, #[allow(unused_variables)] syntax_tree: &'a SyntaxTree) -> $Ref {
                match &self.0 {
                    $( $Impl::$alternative(node_or_token) => {
                        alternation_ref!($Ref syntax_tree node_or_token $alternative $Alternative)
                    } )*
                }
            }

            pub fn grammar() -> &'static Grammar {
                lazy_static! {
                    static ref GRAMMAR: Grammar = Grammar::new::<$T>();
                }
                &GRAMMAR
            }
        }

        impl Buildable for $T {
            fn name() -> &'static str {
                stringify!($T)
            }

            fn rule(#[allow(unused_variables)] grammar: &mut GrammarBuilder) -> RecursiveRule {
                RecursiveRule::Alternation(vec![
                    $( rule!(grammar $Alternative), )*
                ])
            }

            fn build(
                syntax_tree: &mut SyntaxTree,
                reader: &mut NodeBuilderReader,
            ) -> Result<Option<NodeRef>, NodeBuilderError> {
                let node_ref = NodeRef(syntax_tree.nodes.$syntax_tree_name.len());
                $(
                    if let Some(node) = reader.read()? {
                        syntax_tree.nodes.$syntax_tree_name.push(Self($Impl::$alternative(node)));
                        return Ok(Some(node_ref));
                    }
                )*
                Ok(None)
            }
        }

        #[derive(Debug)]
        pub enum $Impl {
            $( $alternative(field_type!($Alternative)), )*
        }

        pub enum $Ref<'a> {
            $( $alternative(&'a ref_field_type!($Alternative)), )*
        }
    };
}

macro_rules! token_alternation {
    { $syntax_tree_name:ident: $T:ident {
        $( $alternative:ident $( $alternative_node:ident )?: $Alternative:tt, )*
    } } => {
        #[derive(Debug)]
        pub enum $T {
            $( $alternative(field_type!($Alternative)), )*
        }

        impl $T {
            pub fn grammar() -> &'static Grammar {
                lazy_static! {
                    static ref GRAMMAR: Grammar = Grammar::new::<$T>();
                }
                &GRAMMAR
            }
        }

        impl Buildable for $T {
            fn name() -> &'static str {
                stringify!($T)
            }

            fn rule(#[allow(unused_variables)] grammar: &mut GrammarBuilder) -> RecursiveRule {
                RecursiveRule::Alternation(vec![
                    $( rule!(grammar $Alternative), )*
                ])
            }

            fn build(
                syntax_tree: &mut SyntaxTree,
                reader: &mut NodeBuilderReader,
            ) -> Result<Option<NodeRef>, NodeBuilderError> {
                let node_ref = NodeRef(syntax_tree.nodes.$syntax_tree_name.len());
                $(
                    if let Some(node) = reader.read()? {
                        syntax_tree.nodes.$syntax_tree_name.push(Self::$alternative(node));
                        return Ok(Some(node_ref));
                    }
                )*
                Ok(None)
            }
        }
    };
}

macro_rules! repetition_field_type {
    ( $rule:ident ) => {
        Vec<Token>
    };
    ( ( $nodes:ident: $T:ty ) ) => {
        Vec<NodeRef>
    };
}

macro_rules! repetition_accessor {
    ( $field:ident $token_kind:ident ) => {
        pub fn $field(&self) -> &[Token] {
            &self.$field
        }
    };
    ( $field:ident ( $nodes:ident: $T:ty ) ) => {
        pub fn $field<'a>(
            &'a self,
            syntax_tree: &'a SyntaxTree,
        ) -> impl ExactSizeIterator<Item = &$T> {
            self.$field
                .iter()
                .map(|&NodeRef(i)| &syntax_tree.nodes.$nodes[i])
        }
    };
}

macro_rules! repetition_helper {
    ( $syntax_tree_name:ident $T:ident $repetition_kind:ident $field:ident $Field:tt ) => {
        #[derive(Debug)]
        pub struct $T {
            $field: repetition_field_type!($Field),
        }

        impl $T {
            repetition_accessor!($field $Field);

            pub fn grammar() -> &'static Grammar {
                lazy_static! {
                    static ref GRAMMAR: Grammar = Grammar::new::<$T>();
                }
                &GRAMMAR
            }
        }

        impl Buildable for $T {
            fn name() -> &'static str {
                stringify!($T)
            }

            fn rule(#[allow(unused_variables)] grammar: &mut GrammarBuilder) -> RecursiveRule {
                RecursiveRule::$repetition_kind(rule!(grammar $Field))
            }

            fn build(
                syntax_tree: &mut SyntaxTree,
                reader: &mut NodeBuilderReader,
            ) -> Result<Option<NodeRef>, NodeBuilderError> {
                let node_ref = NodeRef(syntax_tree.nodes.$syntax_tree_name.len());
                syntax_tree.nodes.$syntax_tree_name.push(Self {
                    $field: reader.read()?,
                });
                Ok(Some(node_ref))
            }
        }
    };
}

macro_rules! global_repetition {
    ( $syntax_tree_name:ident: $T:ident => $field:ident: $Field:tt ) => {
        repetition_helper!($syntax_tree_name $T GlobalRepetition $field $Field);
    };
}

macro_rules! repetition {
    ( $syntax_tree_name:ident: $T:ident => $field:ident: $Field:tt ) => {
        repetition_helper!($syntax_tree_name $T Repetition $field $Field);
    };
}

macro_rules! braced_repetition_helper {
    ( $syntax_tree_name:ident $T:ident $opening:ident $closing:ident $brace_kind:ident $field:ident $Field:tt ) => {
        #[derive(Debug)]
        pub struct $T {
            $opening: Token,
            $field: repetition_field_type!($Field),
            $closing: Token,
        }

        impl $T {
            pub fn $opening(&self) -> Token {
                self.$opening
            }

            repetition_accessor!($field $Field);

            pub fn $closing(&self) -> Token {
                self.$closing
            }

            pub fn grammar() -> &'static Grammar {
                lazy_static! {
                    static ref GRAMMAR: Grammar = Grammar::new::<$T>();
                }
                &GRAMMAR
            }
        }

        impl Buildable for $T {
            fn name() -> &'static str {
                stringify!($T)
            }

            #[allow(unused_variables)]
            fn rule(#[allow(unused_variables)] grammar: &mut GrammarBuilder) -> RecursiveRule {
                RecursiveRule::BracedRepetition(BraceKind::$brace_kind, rule!(grammar $Field))
            }

            fn build(
                syntax_tree: &mut SyntaxTree,
                reader: &mut NodeBuilderReader,
            ) -> Result<Option<NodeRef>, NodeBuilderError> {
                let node_ref = NodeRef(syntax_tree.nodes.$syntax_tree_name.len());
                syntax_tree.nodes.$syntax_tree_name.push(Self {
                    $opening: reader.read()?,
                    $field: reader.read()?,
                    $closing: reader.read()?,
                });
                Ok(Some(node_ref))
            }
        }
    };
}

macro_rules! braced_repetition {
    ( $syntax_tree_name:ident: $T:ident => ( $field:ident: $Field:tt ) ) => {
        braced_repetition_helper!($syntax_tree_name $T opening_paren closing_paren Paren $field $Field);
    };
    ( $syntax_tree_name:ident: $T:ident => [ $field:ident: $Field:tt ] ) => {
        braced_repetition_helper!($syntax_tree_name $T opening_brack closing_brack Brack $field $Field);
    };
    ( $syntax_tree_name:ident: $T:ident => { $field:ident: $Field:tt } ) => {
        braced_repetition_helper!($syntax_tree_name $T opening_curly closing_curly Curly $field $Field);
    };
}

alternation! { argument_expressions: ArgumentExpression ArgumentExpressionImpl ArgumentExpressionRef {
    False: False,
    True: True,
    Integer: Integer,
    Float: Float,
    Ident: Ident,
    Tuple: (tuple_literals: TupleLiteral),
    Struct: (struct_literals: StructLiteral),
    Array: (array_literals: ArrayLiteral),
    Set: (set_literals: SetLiteral),
    Map: (map_literals: MapLiteral),
    Return: (return_expressions: ReturnExpression),
    Break: (break_expressions: BreakExpression),
    Continue: (continue_expressions: ContinueExpression),
    Group: (group_expressions: GroupExpression),
    IfBlock: (if_blocks: IfBlock),
    WhileBlock: (while_blocks: WhileBlock),
    ForBlock: (for_blocks: ForBlock),
    LoopBlock: (loop_blocks: LoopBlock),
    MatchBlock: (match_blocks: MatchBlock),
    // Type last, as it has overlap with literals which must be resolved using the `type` prefix.
    Type: (type_expressions: TypeExpression),
} }

braced_repetition!(array_literals: ArrayLiteral => [ items: (trailing_comma_expressions: TrailingCommaExpression) ]);

concatenation! { binary_operations: BinaryOperation {
    > operator: (binary_operators: BinaryOperator),
    expression: (expressions: Expression),
} }

token_alternation! { binary_operators: BinaryOperator {
    Add: Plus,
    Sub: Minus,
    Mul: Star,
    Div: Slash,
    Rem: Percent,
    BitAnd: And,
    BitOr: Or,
    BitXor: Caret,
    Shl: Shl,
    Shr: Shr,
    Eq: EqEq,
    Ne: Ne,
    Gt: Gt,
    Lt: Lt,
    Ge: Ge,
    Le: Le,
    And: AndAnd,
    Or: OrOr,
    Assign: Eq,
    AddAssign: PlusEq,
    SubAssign: MinusEq,
    MulAssign: StarEq,
    DivAssign: SlashEq,
    RemAssign: PercentEq,
    BitAndAssign: AndEq,
    BitOrAssign: OrEq,
    BitXorAssign: CaretEq,
    ShlAssign: ShlEq,
    ShrAssign: ShrEq,
} }

braced_repetition!(blocks: Block => { statements: (statements: Statement) });

concatenation! { break_expressions: BreakExpression {
    > break_kw: Break,
    label: [Label],
    expression: [expressions: Expression],
} }

alternation! { conditions: Condition ConditionImpl ConditionRef {
    LetBinding: (let_bindings: LetBinding),
    Expression: (expressions: Expression),
} }

concatenation! { continue_expressions: ContinueExpression {
    > continue_kw: Continue,
    label: [Label],
} }

concatenation! { else_blocks: ElseBlock {
    > else_kw: Else,
    block: (blocks: Block),
} }

concatenation! { else_if_blocks: ElseIfBlock {
    else_kw: Else,
    > if_kw: If,
    condition: (conditions: Condition),
    block: (blocks: Block),
} }

repetition!(else_if_chains: ElseIfChain => else_if_blocks: (else_if_blocks: ElseIfBlock));

concatenation! { expressions: Expression {
    prefix_operation_chain: (prefix_operation_chains: PrefixOperationChain),
    > root: (root_expressions: RootExpression),
    suffix_operation_chain: (suffix_operation_chains: SuffixOperationChain),
} }

concatenation! { for_blocks: ForBlock {
    label: [Label],
    > for_kw: For,
    pattern: (patterns: Pattern),
    in_kw: In,
    expression: (expressions: Expression),
    block: (blocks: Block),
} }

concatenation! { functions: Function {
    pub_kw: [Pub],
    > fn_kw: Fn,
    name: Ident,
    pattern: (patterns: Pattern),
    return_type: [return_types: ReturnType],
    block: (blocks: Block),
} }

concatenation! { group_expressions: GroupExpression {
    > opening_paren: LParen,
    expression: (expressions: Expression),
    closing_paren: RParen,
} }

concatenation! { if_blocks: IfBlock {
    > if_kw: If,
    condition: (conditions: Condition),
    block: (blocks: Block),
    else_if_chain: (else_if_chains: ElseIfChain),
    else_block: [else_blocks: ElseBlock],
} }

alternation! { items: Item ItemImpl ItemRef {
    Function: (functions: Function),
} }

concatenation! { labeled_blocks: LabeledBlock {
    label: [Label],
    > block: (blocks: Block),
} }

concatenation! { let_bindings: LetBinding {
    > let_kw: Let,
    pattern: (patterns: Pattern),
    eq: Eq,
    expression: (expressions: Expression),
} }

concatenation! { loop_blocks: LoopBlock {
    label: [Label],
    > loop_kw: Loop,
    block: (blocks: Block),
} }

concatenation! { map_entries: MapEntry {
    key: (expressions: Expression),
    > colon: Colon,
    value: (expressions: Expression),
    comma: [Comma],
} }

braced_repetition!(map_entry_chains: MapEntryChain => { entries: (map_entries: MapEntry) });

concatenation! { map_literals: MapLiteral {
    at: At,
    > content: (map_entry_chains: MapEntryChain),
} }

concatenation! { match_arms: MatchArm {
    > pattern: (patterns: Pattern),
    fat_arrow: FatArrow,
    expression: (expressions: Expression),
    comma: [Comma],
} }

concatenation! { match_blocks: MatchBlock {
    > match_kw: Match,
    expression: (expressions: Expression),
    block: (match_bodies: MatchBody),
} }

braced_repetition!(match_bodies: MatchBody => { arms: (match_arms: MatchArm) });

global_repetition!(modules: Module => item: (items: Item));

concatenation! { path_resolutions: PathResolution {
    > path_sep: PathSep,
    name: Ident,
} }

repetition!(path_resolution_chains: PathResolutionChain => chain: (path_resolutions: PathResolution));

alternation! { patterns: Pattern PatternImpl PatternRef {
    Discard: Underscore,
    Ident: Ident,
} }

alternation! { prefix_operations: PrefixOperation PrefixOperationImpl PrefixOperationRef {
    UnaryOperator: (unary_operators: UnaryOperator),
} }

repetition!(prefix_operation_chains: PrefixOperationChain => operations: (prefix_operations: PrefixOperation));

concatenation! { return_expressions: ReturnExpression {
    > return_kw: Return,
    expression: [expressions: Expression],
} }

concatenation! { return_types: ReturnType {
    > r_arrow: RArrow,
    return_type: (types: Type),
} }

alternation! { root_expressions: RootExpression RootExpressionImpl RootExpressionRef {
    Block: (labeled_blocks: LabeledBlock),
    Argument: (argument_expressions: ArgumentExpression),
} }

braced_repetition!(set_item_chains: SetItemChain => { items: (trailing_comma_expressions: TrailingCommaExpression) });

concatenation! { set_literals: SetLiteral {
    pound: Pound,
    > content: (set_item_chains: SetItemChain),
} }

alternation! { statements: Statement StatementImpl StatementRef {
    Semi: Semi,
    LetBinding: (let_bindings: LetBinding),
    Expression: (expressions: Expression),
} }

concatenation! { struct_fields: StructField {
    name: [struct_field_names: StructFieldName],
    > expression: (expressions: Expression),
    comma: [Comma],
} }

braced_repetition!(struct_field_chains: StructFieldChain => ( fields: (struct_fields: StructField) ));

concatenation! { struct_field_names: StructFieldName {
    name: Ident,
    > colon: Colon,
} }

concatenation! { struct_literals: StructLiteral {
    at: At,
    > content: (struct_field_chains: StructFieldChain),
} }

alternation! { suffix_operations: SuffixOperation SuffixOperationImpl SuffixOperationRef {
    Binary: (binary_operations: BinaryOperation),
    Call: (argument_expressions: ArgumentExpression),
} }

repetition!(suffix_operation_chains: SuffixOperationChain => operations: (suffix_operations: SuffixOperation));

concatenation! { trailing_comma_expressions: TrailingCommaExpression {
    > expression: (expressions: Expression),
    comma: [Comma],
} }

concatenation! { tuple_fields: TupleField {
    index: [tuple_indices: TupleIndex],
    > expression: (expressions: Expression),
    comma: [Comma],
} }

braced_repetition!(tuple_field_chains: TupleFieldChain => ( fields: (tuple_fields: TupleField) ));

concatenation! { tuple_indices: TupleIndex {
    index: Integer,
    > colon: Colon,
} }

concatenation! { tuple_literals: TupleLiteral {
    pound: Pound,
    > content: (tuple_field_chains: TupleFieldChain),
} }

concatenation! { types: Type {
    leading_path_sep: [PathSep],
    > name: Ident,
    chain: (path_resolution_chains: PathResolutionChain),
} }

concatenation! { type_expressions: TypeExpression {
    type_kw: [Type],
    > ty: (types: Type),
} }

token_alternation! { unary_operators: UnaryOperator {
    Neg: Minus,
    Not: Not,
} }

concatenation! { while_blocks: WhileBlock {
    label: [Label],
    > while_kw: While,
    condition: (conditions: Condition),
    block: (blocks: Block),
} }
