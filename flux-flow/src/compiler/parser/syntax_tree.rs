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
    binary_operator: Vec<BinaryOperator>,
    block: Vec<Block>,
    compound_assignment_operator: Vec<CompoundAssignmentOperator>,
    expression: Vec<Expression>,
    for_block: Vec<ForBlock>,
    if_block: Vec<IfBlock>,
    else_if_block: Vec<ElseIfBlock>,
    else_if_chain: Vec<ElseIfChain>,
    else_block: Vec<ElseBlock>,
    while_block: Vec<WhileBlock>,
    function: Vec<Function>,
    condition: Vec<Condition>,
    item: Vec<Item>,
    let_binding: Vec<LetBinding>,
    loop_block: Vec<LoopBlock>,
    module: Vec<Module>,
    statement: Vec<Statement>,
    unary_operator: Vec<UnaryOperator>,
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
        self.nodes.module.last().expect("root module should exist")
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
    ( $grammar:ident ( ref $T:ty ) ) => {
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
    ( $grammar:ident ( ref $T:ty ) ) => {
        OptionalRule::Required(Rule::Ref($grammar.rule::<$T>()))
    };
    ( $grammar:ident [ ref $T:ty ] ) => {
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
    ( ( ref $T:ty ) ) => {
        NodeRef
    };
    ( [ ref $T:ty ] ) => {
        Option<NodeRef>
    };
}

macro_rules! ref_field_type {
    ( $rule:ident ) => {
        Token
    };
    ( ( ref $T:ty ) ) => {
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
    ( $field:ident ( ref $T:ty ) ) => {
        pub fn $field<'a>(&self, syntax_tree: &'a SyntaxTree) -> &'a $T {
            &syntax_tree.nodes.$field[self.$field.0]
        }
    };
    ( $field:ident [ ref $T:ty ] ) => {
        pub fn $field<'a>(&self, syntax_tree: &'a SyntaxTree) -> Option<&'a $T> {
            self.$field.map(|NodeRef(i)| &syntax_tree.nodes.$field[i])
        }
    };
}

// TODO: field name currently has to match the SyntaxTreeNodes name
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

        impl $T {
            $( field_accessor!($essential_fields $EssentialFields); )*
            field_accessor!($last_essential_field $LastEssentialField);
            $( field_accessor!($required_fields $RequiredFields); )*
        }
    };
}

macro_rules! alternation_ref {
    ( $Ref:ident $syntax_tree:ident $node:ident $alternative:ident $alternative_node:ident ) => {
        $Ref::$alternative(&$syntax_tree.nodes.$alternative_node[$node.0])
    };
    ( $Ref:ident $syntax_tree:ident $token:ident $alternative:ident ) => {
        $Ref::$alternative($token)
    };
}

// TODO: Move SyntaxTreeNodes name over to the right side similar to what I want to do with concatenations
macro_rules! alternation {
    { $syntax_tree_name:ident: $T:ident $Impl:ident $Ref:ident {
        $( $alternative:ident $( $alternative_node:ident )?: $Alternative:tt, )*
    } } => {
        #[derive(Debug)]
        pub struct $T(pub $Impl);

        impl $T {
            pub fn get<'a>(&'a self, #[allow(unused_variables)] syntax_tree: &'a SyntaxTree) -> $Ref {
                match &self.0 {
                    $( $Impl::$alternative(node_or_token) => {
                        alternation_ref!($Ref syntax_tree node_or_token $alternative $( $alternative_node )?)
                    } )*
                }
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
    ( ( ref $T:ty ) ) => {
        Vec<NodeRef>
    };
}

macro_rules! repetition_accessor {
    ( $field:ident $fields:ident $token_kind:ident ) => {
        pub fn $fields(&self) -> &[Token] {
            &self.$fields
        }
    };
    ( $field:ident $fields:ident ( ref $T:ty ) ) => {
        pub fn $fields<'a>(
            &'a self,
            syntax_tree: &'a SyntaxTree,
        ) -> impl ExactSizeIterator<Item = &$T> {
            self.$fields
                .iter()
                .map(|&NodeRef(i)| &syntax_tree.nodes.$field[i])
        }
    };
}

macro_rules! repetition_helper {
    ( $syntax_tree_name:ident $T:ident $repetition_kind:ident $field:ident $fields:ident $Field:tt ) => {
        #[derive(Debug)]
        pub struct $T {
            $fields: repetition_field_type!($Field),
        }

        impl $T {
            repetition_accessor!($field $fields $Field);
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
                    $fields: reader.read()?,
                });
                Ok(Some(node_ref))
            }
        }
    };
}

macro_rules! global_repetition {
    ( $syntax_tree_name:ident: $T:ident => $field:ident / $fields:ident: $Field:tt ) => {
        repetition_helper!($syntax_tree_name $T GlobalRepetition $field $fields $Field);
    };
}

macro_rules! repetition {
    ( $syntax_tree_name:ident: $T:ident => $field:ident / $fields:ident: $Field:tt ) => {
        repetition_helper!($syntax_tree_name $T Repetition $field $fields $Field);
    };
}

macro_rules! braced_repetition_helper {
    ( $syntax_tree_name:ident $T:ident $opening:ident $closing:ident $brace_kind:ident $field:ident $fields:ident $Field:tt ) => {
        #[derive(Debug)]
        pub struct $T {
            $opening: Token,
            $fields: repetition_field_type!($Field),
            $closing: Token,
        }

        impl $T {
            pub fn $opening(&self) -> Token {
                self.$opening
            }

            repetition_accessor!($field $fields $Field);

            pub fn $closing(&self) -> Token {
                self.$closing
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
                    $fields: reader.read()?,
                    $closing: reader.read()?,
                });
                Ok(Some(node_ref))
            }
        }
    };
}

macro_rules! braced_repetition {
    ( $syntax_tree_name:ident: $T:ident => ( $field:ident / $fields:ident: $Field:tt ) ) => {
        braced_repetition_helper!($syntax_tree_name $T opening_paren closing_paren Paren $field $fields $Field);
    };
    ( $syntax_tree_name:ident: $T:ident => [ $field:ident / $fields:ident: $Field:tt ] ) => {
        braced_repetition_helper!($syntax_tree_name $T opening_brack closing_brack Brack $field $fields $Field);
    };
    ( $syntax_tree_name:ident: $T:ident => { $field:ident / $fields:ident: $Field:tt } ) => {
        braced_repetition_helper!($syntax_tree_name $T opening_curly closing_curly Curly $field $fields $Field);
    };
}

impl Module {
    pub fn grammar() -> &'static Grammar {
        lazy_static! {
            static ref GRAMMAR: Grammar = Grammar::new::<Module>();
        }
        &GRAMMAR
    }
}

token_alternation! { binary_operator: BinaryOperator {
    // Math
    Plus: Plus,
    Minus: Minus,
    Multiply: Star,
    Divide: Slash,
    Modulus: Percent,

    // Bit
    BitAnd: And,
    BitOr: Or,
    BitXor: Caret,
    Shl: Shl,
    Shr: Shr,

    // Logic
    Eq: EqEq,
    Ne: Ne,
    Gt: Gt,
    Lt: Lt,
    Ge: Ge,
    Le: Le,
    LogicAnd: AndAnd,
    LogicOr: OrOr,
} }

braced_repetition!(block: Block => { statement / statements: (ref Statement) });

token_alternation! { compound_assignment_operator: CompoundAssignmentOperator {
    // Math
    Plus: PlusEq,
    Minus: MinusEq,
    Multiply: StarEq,
    Divide: SlashEq,
    Modulus: PercentEq,

    // Bit
    BitAnd: AndEq,
    BitOr: OrEq,
    BitXor: CaretEq,
    Shl: ShlEq,
    Shr: ShrEq,
} }

alternation! { expression: Expression ExpressionImpl ExpressionRef {
    Block block: (ref Block),
    IfBlock if_block: (ref IfBlock),
    WhileBlock while_block: (ref WhileBlock),
    ForBlock for_block: (ref ForBlock),
    LoopBlock loop_block: (ref LoopBlock),
} }

alternation! { statement: Statement StatementImpl StatementRef {
    Semi: Semi,
    LetBinding let_binding: (ref LetBinding),
    Expression expression: (ref Expression),
} }

concatenation! { if_block: IfBlock {
    > if_kw: If,
    condition: (ref Condition),
    block: (ref Block),
    else_if_chain: (ref ElseIfChain),
    else_block: [ref ElseBlock],
} }

repetition!(else_if_chain: ElseIfChain => else_if_block / else_if_blocks: (ref ElseIfBlock));

concatenation! { else_if_block: ElseIfBlock {
    else_kw: Else,
    > if_kw: If,
    condition: (ref Condition),
    block: (ref Block),
} }

concatenation! { else_block: ElseBlock {
    > else_kw: Else,
    block: (ref Block),
} }

concatenation! { while_block: WhileBlock {
    > while_kw: While,
    condition: (ref Condition),
    block: (ref Block),
} }

alternation! { condition: Condition ConditionImpl ConditionRef {
    LetBinding let_binding: (ref LetBinding),
    Expression expression: (ref Expression),
} }

concatenation! { loop_block: LoopBlock {
    > loop_kw: Loop,
    block: (ref Block),
} }

concatenation! { for_block: ForBlock {
    > for_kw: For,
    // TODO: pattern
    in_kw: In,
    // TODO: expression
    block: (ref Block),
} }

concatenation! { function: Function {
    pub_kw: [Pub],
    > fn_kw: Fn,
    name: Ident,
    block: (ref Block),
} }

alternation! { item: Item ItemImpl ItemRef {
    Function function: (ref Function),
} }

concatenation! { let_binding: LetBinding {
    > let_kw: Let,
    lhs: Ident,
    eq: Eq,
    rhs: Ident,
} }

global_repetition!(module: Module => item / items: (ref Item));

token_alternation! { unary_operator: UnaryOperator {
    Neg: Minus,
    Not: Not,
} }
