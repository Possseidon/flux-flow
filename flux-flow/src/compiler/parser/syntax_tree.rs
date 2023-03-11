use std::{num::NonZeroUsize, ops::Range};

use lazy_static::lazy_static;

use crate::compiler::lexer::{BraceKind, TokenKind};

use super::{
    grammar::{Grammar, GrammarBuilder, OptionalRule, RecursiveRule, Rule},
    syntax_tree_node_builder::{Buildable, NodeBuilderError, NodeBuilderReader},
};

// TODO: Don't create node refs for empty repetitions.

#[derive(Debug)]
pub struct SyntaxTree {
    initial_whitespace: usize,
    nodes: SyntaxTreeNodes,
}

#[derive(Debug, Default)]
struct SyntaxTreeNodes {
    argument_expressions: Vec<ArgumentExpression>,
    array_literals: Vec<ArrayLiteral>,
    array_updaters: Vec<ArrayUpdater>,
    binary_operations: Vec<BinaryOperation>,
    binary_operators: Vec<BinaryOperator>,
    blocks: Vec<Block>,
    break_expressions: Vec<BreakExpression>,
    collection_modifier_chains: Vec<CollectionModifierChain>,
    collection_modifier_kinds: Vec<CollectionModifierKind>,
    collection_modifiers: Vec<CollectionModifier>,
    conditions: Vec<Condition>,
    continue_expressions: Vec<ContinueExpression>,
    else_blocks: Vec<ElseBlock>,
    else_if_blocks: Vec<ElseIfBlock>,
    else_if_chains: Vec<ElseIfChain>,
    expression_groups: Vec<ExpressionGroup>,
    expressions: Vec<Expression>,
    for_blocks: Vec<ForBlock>,
    functions: Vec<Function>,
    if_blocks: Vec<IfBlock>,
    index_expressions: Vec<IndexExpression>,
    index_operations: Vec<IndexOperation>,
    index_operators: Vec<IndexOperator>,
    items: Vec<Item>,
    key_values: Vec<KeyValue>,
    labeled_blocks: Vec<LabeledBlock>,
    let_statements: Vec<LetStatement>,
    loop_blocks: Vec<LoopBlock>,
    map_entry_chains: Vec<MapEntryChain>,
    map_literals: Vec<MapLiteral>,
    match_arms: Vec<MatchArm>,
    match_blocks: Vec<MatchBlock>,
    match_bodies: Vec<MatchBody>,
    modules: Vec<Module>,
    path_resolution_chains: Vec<PathResolutionChain>,
    path_resolutions: Vec<PathResolution>,
    paths: Vec<Path>,
    patterns: Vec<Pattern>,
    prefix_operation_chains: Vec<PrefixOperationChain>,
    prefix_operations: Vec<PrefixOperation>,
    return_expressions: Vec<ReturnExpression>,
    return_types: Vec<ReturnType>,
    root_expressions: Vec<RootExpression>,
    root_types: Vec<RootType>,
    set_item_chains: Vec<SetItemChain>,
    set_literals: Vec<SetLiteral>,
    statements: Vec<Statement>,
    struct_field_chains: Vec<StructFieldChain>,
    struct_field_names: Vec<StructFieldName>,
    struct_field_type_chains: Vec<StructFieldTypeChain>,
    struct_field_types: Vec<StructFieldType>,
    struct_fields: Vec<StructField>,
    struct_literals: Vec<StructLiteral>,
    struct_types: Vec<StructType>,
    suffix_expressions: Vec<SuffixExpression>,
    suffix_operation_chains: Vec<SuffixOperationChain>,
    suffix_operations: Vec<SuffixOperation>,
    trailing_comma_expressions: Vec<TrailingCommaExpression>,
    tuple_field_chains: Vec<TupleFieldChain>,
    tuple_field_type_chains: Vec<TupleFieldTypeChain>,
    tuple_field_types: Vec<TupleFieldType>,
    tuple_fields: Vec<TupleField>,
    tuple_indices: Vec<TupleIndex>,
    tuple_literals: Vec<TupleLiteral>,
    tuple_types: Vec<TupleType>,
    type_annotations: Vec<TypeAnnotation>,
    type_expressions: Vec<TypeExpression>,
    type_groups: Vec<TypeGroup>,
    types: Vec<Type>,
    unary_operators: Vec<UnaryOperator>,
    unit_shorthands: Vec<UnitShorthand>,
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

    pub fn visualize(&self, code: &str) {
        self.root_module().visualize(self, code, 0, false);
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
    // token_len first to allow size optimization for OptionalToken
    pub token_len: NonZeroUsize,
    pub token_start: usize,
    pub trailing_whitespace_len: usize,
}

impl Token {
    pub fn token_end(&self) -> usize {
        self.token_start + self.token_len.get()
    }

    pub fn trailing_whitespace_start(&self) -> usize {
        self.token_end()
    }

    pub fn trailing_whitespace_end(&self) -> usize {
        self.trailing_whitespace_start() + self.trailing_whitespace_len
    }

    pub fn token(&self) -> Range<usize> {
        self.token_start..self.token_end()
    }

    pub fn trailing_whitespace(&self) -> Range<usize> {
        self.trailing_whitespace_start()..self.trailing_whitespace_end()
    }

    pub fn token_and_trailing_whitespace(&self) -> Range<usize> {
        self.token_start..self.trailing_whitespace_end()
    }

    pub fn visualize(
        &self,
        _syntax_tree: &SyntaxTree,
        code: &str,
        indent: usize,
        with_indent: bool,
    ) {
        println!(
            "{: <2$}`{}`",
            "",
            &code[self.token()],
            if with_indent { indent } else { 0 }
        );
    }
}

#[derive(Clone, Copy, Debug)]
pub struct EmptyToken {
    pub index: usize,
}

#[derive(Clone, Copy, Debug)]
pub enum OptionalToken {
    Some(Token),
    None(EmptyToken),
}

impl OptionalToken {
    pub fn some(
        token_start: usize,
        token_len: NonZeroUsize,
        trailing_whitespace_len: usize,
    ) -> Self {
        Self::Some(Token {
            token_start,
            token_len,
            trailing_whitespace_len,
        })
    }

    pub fn none(index: usize) -> Self {
        Self::None(EmptyToken { index })
    }

    pub fn is_some(&self) -> bool {
        matches!(self, Self::Some(..))
    }

    pub fn as_some(&self) -> Option<&Token> {
        if let Self::Some(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn try_into_some(self) -> Result<Token, Self> {
        if let Self::Some(token) = self {
            Ok(token)
        } else {
            Err(self)
        }
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Self::None(..))
    }

    pub fn as_none(&self) -> Option<&EmptyToken> {
        if let Self::None(empty_token) = self {
            Some(empty_token)
        } else {
            None
        }
    }

    pub fn try_into_none(self) -> Result<EmptyToken, Self> {
        if let Self::None(empty_token) = self {
            Ok(empty_token)
        } else {
            Err(self)
        }
    }

    pub fn token_start(&self) -> usize {
        match self {
            OptionalToken::Some(token) => token.token_start,
            OptionalToken::None(empty_token) => empty_token.index,
        }
    }

    pub fn token_len(&self) -> usize {
        match self {
            OptionalToken::Some(token) => token.token_len.get(),
            OptionalToken::None(..) => 0,
        }
    }

    pub fn token_end(&self) -> usize {
        match self {
            OptionalToken::Some(token) => token.token_end(),
            OptionalToken::None(empty_token) => empty_token.index,
        }
    }

    pub fn trailing_whitespace_start(&self) -> usize {
        match self {
            OptionalToken::Some(token) => token.trailing_whitespace_start(),
            OptionalToken::None(empty_token) => empty_token.index,
        }
    }

    pub fn trailing_whitespace_len(&self) -> usize {
        match self {
            OptionalToken::Some(token) => token.trailing_whitespace_len,
            OptionalToken::None(..) => 0,
        }
    }

    pub fn trailing_whitespace_end(&self) -> usize {
        match self {
            OptionalToken::Some(token) => token.trailing_whitespace_end(),
            OptionalToken::None(empty_token) => empty_token.index,
        }
    }

    pub fn token(&self) -> Range<usize> {
        match self {
            OptionalToken::Some(token) => token.token(),
            OptionalToken::None(empty_token) => empty_token.index..empty_token.index,
        }
    }

    pub fn trailing_whitespace(&self) -> Range<usize> {
        match self {
            OptionalToken::Some(token) => token.trailing_whitespace(),
            OptionalToken::None(empty_token) => empty_token.index..empty_token.index,
        }
    }

    pub fn token_and_trailing_whitespace(&self) -> Range<usize> {
        match self {
            OptionalToken::Some(token) => token.token_and_trailing_whitespace(),
            OptionalToken::None(empty_token) => empty_token.index..empty_token.index,
        }
    }

    pub fn visualize(
        &self,
        syntax_tree: &SyntaxTree,
        code: &str,
        indent: usize,
        with_indent: bool,
    ) {
        match self {
            OptionalToken::Some(token) => token.visualize(syntax_tree, code, indent, with_indent),
            OptionalToken::None(_) => println!("n/a"),
        }
    }
}

fn print_indent(indent: usize) {
    print!("{: <1$}", "", indent);
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
        OptionalToken
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
        pub fn $field(&self) -> OptionalToken {
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

macro_rules! visualize {
    ( $syntax_tree:ident $code:ident $indent:tt $with_indent:tt $field:tt $rule:ident ) => {
        $field.visualize($syntax_tree, $code, $indent, $with_indent);
    };
    ( $syntax_tree:ident $code:ident $indent:tt $with_indent:tt $field:tt [ $rule:ident ] ) => {
        $field.visualize($syntax_tree, $code, $indent, $with_indent);
    };
    ( $syntax_tree:ident $code:ident $indent:tt $with_indent:tt $field:tt ( $nodes:ident: $T:ty ) ) => {
        $syntax_tree.nodes.$nodes[$field.0].visualize($syntax_tree, $code, $indent, $with_indent);
    };
    ( $syntax_tree:ident $code:ident $indent:tt $with_indent:tt $field:tt [ $nodes:ident: $T:ty ] ) => {
        if let Some(NodeRef(i)) = $field {
            $syntax_tree.nodes.$nodes[i].visualize($syntax_tree, $code, $indent, $with_indent);
        } else {
            println!("n/a");
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

            pub fn visualize(
                &self,
                syntax_tree: &SyntaxTree,
                code: &str,
                mut indent: usize,
                with_indent: bool,
            ) {
                if with_indent { print_indent(indent); }
                println!("{}", <Self as Buildable>::name());
                indent += 1;
                $( print_indent(indent); print!("{}: ", stringify!($essential_fields)); visualize!(syntax_tree code indent false (self.$essential_fields) $EssentialFields); )*
                print_indent(indent); print!("{}: ", stringify!($last_essential_field)); visualize!(syntax_tree code indent false (self.$last_essential_field) $LastEssentialField);
                $( print_indent(indent); print!("{}: ", stringify!($required_fields)); visualize!(syntax_tree code indent false (self.$required_fields) $RequiredFields); )*
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

            pub fn visualize(
                &self,
                syntax_tree: &SyntaxTree,
                code: &str,
                indent: usize,
                with_indent: bool,
            ) {
                if with_indent { print_indent(indent); }
                print!("{}::", <Self as Buildable>::name());
                match &self.0 {
                    $( $Impl::$alternative(node_or_token) => {
                        print!("{} > ", stringify!($alternative));
                        visualize!(syntax_tree code indent false node_or_token $Alternative);
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

        impl $T {
            pub fn grammar() -> &'static Grammar {
                lazy_static! {
                    static ref GRAMMAR: Grammar = Grammar::new::<$T>();
                }
                &GRAMMAR
            }

            pub fn visualize(
                &self,
                syntax_tree: &SyntaxTree,
                code: &str,
                indent: usize,
                with_indent: bool,
            ) {
                if with_indent { print_indent(indent); }
                print!("{}: ", <Self as Buildable>::name());
                match &self {
                    $( Self::$alternative(node_or_token) => {
                        visualize!(syntax_tree code indent false node_or_token $Alternative);
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

            pub fn visualize(
                &self,
                syntax_tree: &SyntaxTree,
                code: &str,
                indent: usize,
                with_indent: bool,
            ) {
                if with_indent { print_indent(indent); }
                print!("{}", <Self as Buildable>::name());
                println!("[{}]", self.$field.len());
                for field in &self.$field {
                    visualize!(syntax_tree code (indent + 1) true field $Field);
                }
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

            pub fn visualize(
                &self,
                syntax_tree: &SyntaxTree,
                code: &str,
                indent: usize,
                with_indent: bool,
            ) {
                if with_indent { print_indent(indent); }
                print!("{}", <Self as Buildable>::name());
                println!("[{}]", self.$field.len());
                for field in &self.$field {
                    visualize!(syntax_tree code (indent + 1) true field $Field);
                }
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
    String: String,
    Char: Char,
    Unit: (unit_shorthands: UnitShorthand),
    Path: (paths: Path),
    Tuple: (tuple_literals: TupleLiteral),
    Struct: (struct_literals: StructLiteral),
    Array: (array_literals: ArrayLiteral),
    Set: (set_literals: SetLiteral),
    Map: (map_literals: MapLiteral),
    Return: (return_expressions: ReturnExpression),
    Break: (break_expressions: BreakExpression),
    Continue: (continue_expressions: ContinueExpression),
    Group: (expression_groups: ExpressionGroup),
    If: (if_blocks: IfBlock),
    While: (while_blocks: WhileBlock),
    For: (for_blocks: ForBlock),
    Loop: (loop_blocks: LoopBlock),
    Match: (match_blocks: MatchBlock),
    TypeWithPrefix: (type_expressions: TypeExpression),
    // Type last, as it has overlap with literals which must be resolved using the `type` prefix.
    Type: (types: Type),
} }

braced_repetition!(array_literals: ArrayLiteral => [ items: (trailing_comma_expressions: TrailingCommaExpression) ]);

braced_repetition!(array_updaters: ArrayUpdater => [ assignments: (key_values: KeyValue) ]);

concatenation! { binary_operations: BinaryOperation {
    > operator: (binary_operators: BinaryOperator),
    expression: (suffix_expressions: SuffixExpression),
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

concatenation! { collection_modifiers: CollectionModifier {
    > opening_brack: LBrack,
    kind: [collection_modifier_kinds: CollectionModifierKind],
    closing_brack: RBrack,
}}

repetition!(collection_modifier_chains: CollectionModifierChain => modifiers: (collection_modifiers: CollectionModifier));

alternation! { collection_modifier_kinds: CollectionModifierKind CollectionModifierKindImpl CollectionModifierKindRef {
    Array: Integer,
    Deque: Tilde,
    Set: Lt,
    Map: (types: Type),
} }

alternation! { conditions: Condition ConditionImpl ConditionRef {
    // TODO: Different from LetStatement
    // Let: (let_statements: LetStatement),
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
    prefix: (prefix_operation_chains: PrefixOperationChain),
    > root: (root_expressions: RootExpression),
    suffix: (suffix_operation_chains: SuffixOperationChain),
} }

concatenation! { expression_groups: ExpressionGroup {
    > opening_paren: LParen,
    expression: (expressions: Expression),
    closing_paren: RParen,
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

concatenation! { if_blocks: IfBlock {
    > if_kw: If,
    condition: (conditions: Condition),
    block: (blocks: Block),
    else_if_chain: (else_if_chains: ElseIfChain),
    else_block: [else_blocks: ElseBlock],
} }

alternation! { index_expressions: IndexExpression IndexExpressionImpl IndexExpressionRef {
    Index: Integer,
    Name: Ident,
    TupleUpdate: (tuple_literals: TupleLiteral),
    StructUpdate: (struct_literals: StructLiteral),
    ArrayUpdate: (array_updaters: ArrayUpdater),
    MapUpdate: (map_literals: MapLiteral),
} }

concatenation! { index_operations: IndexOperation {
    > operator: (index_operators: IndexOperator),
    expression: (index_expressions: IndexExpression),
} }

token_alternation! { index_operators: IndexOperator {
    Dot: Dot,
    DotAssign: DotEq,
} }

alternation! { items: Item ItemImpl ItemRef {
    Function: (functions: Function),
} }

concatenation! { key_values: KeyValue {
    > key: (expressions: Expression),
    colon: Colon,
    value: (expressions: Expression),
    comma: [Comma],
} }

concatenation! { labeled_blocks: LabeledBlock {
    label: [Label],
    > block: (blocks: Block),
} }

concatenation! { let_statements: LetStatement {
    > let_kw: Let,
    name: Ident,
    type_annotation: [type_annotations: TypeAnnotation],
    eq: Eq,
    expression: (expressions: Expression),
} }

concatenation! { loop_blocks: LoopBlock {
    label: [Label],
    > loop_kw: Loop,
    block: (blocks: Block),
} }

braced_repetition!(map_entry_chains: MapEntryChain => { entries: (key_values: KeyValue) });

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

global_repetition!(modules: Module => items: (items: Item));

concatenation! { paths: Path {
    leading_path_sep: [PathSep],
    > name: Ident,
    chain: (path_resolution_chains: PathResolutionChain),
} }

concatenation! { path_resolutions: PathResolution {
    > path_sep: PathSep,
    name: Ident,
} }

repetition!(path_resolution_chains: PathResolutionChain => chain: (path_resolutions: PathResolution));

alternation! { patterns: Pattern PatternImpl PatternRef {
    Discard: Underscore,
    Ident: Ident,
    Unit: (unit_shorthands: UnitShorthand),
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
    ty: (types: Type),
} }

alternation! { root_expressions: RootExpression RootExpressionImpl RootExpressionRef {
    Block: (labeled_blocks: LabeledBlock),
    Argument: (argument_expressions: ArgumentExpression),
} }

alternation! { root_types: RootType RootTypeImpl RootTypeRef {
    Deduce: Underscore,
    Never: Not,
    Path: (paths: Path),
    Tuple: (tuple_types: TupleType),
    Struct: (struct_types: StructType),
    Unit: (unit_shorthands: UnitShorthand),
    Group: (type_groups: TypeGroup),
} }

braced_repetition!(set_item_chains: SetItemChain => { items: (trailing_comma_expressions: TrailingCommaExpression) });

concatenation! { set_literals: SetLiteral {
    pound: Pound,
    > content: (set_item_chains: SetItemChain),
} }

alternation! { statements: Statement StatementImpl StatementRef {
    Semi: Semi,
    Let: (let_statements: LetStatement),
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

concatenation! { struct_field_types: StructFieldType {
    name: (struct_field_names: StructFieldName),
    > ty: (types: Type),
    comma: [Comma],
} }

braced_repetition!(struct_field_type_chains: StructFieldTypeChain => ( fields: (struct_field_types: StructFieldType) ));

concatenation! { struct_literals: StructLiteral {
    at: At,
    > content: (struct_field_chains: StructFieldChain),
} }

concatenation! { struct_types: StructType {
    at: At,
    > types: (struct_field_type_chains: StructFieldTypeChain),
} }

concatenation! { suffix_expressions: SuffixExpression {
    prefix: (prefix_operation_chains: PrefixOperationChain),
    > root: (root_expressions: RootExpression),
} }

alternation! { suffix_operations: SuffixOperation SuffixOperationImpl SuffixOperationRef {
    Binary: (binary_operations: BinaryOperation),
    Index: (index_operations: IndexOperation),
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

concatenation! { tuple_field_types: TupleFieldType {
    index: [tuple_indices: TupleIndex],
    > ty: (types: Type),
    comma: [Comma],
} }

braced_repetition!(tuple_field_type_chains: TupleFieldTypeChain => ( fields: (tuple_field_types: TupleFieldType) ));

concatenation! { tuple_indices: TupleIndex {
    index: Integer,
    > colon: Colon,
} }

concatenation! { tuple_literals: TupleLiteral {
    pound: Pound,
    > content: (tuple_field_chains: TupleFieldChain),
} }

concatenation! { tuple_types: TupleType {
    pound: Pound,
    > types: (tuple_field_type_chains: TupleFieldTypeChain),
} }

concatenation! { types: Type {
    collection_modifiers: (collection_modifier_chains: CollectionModifierChain),
    > root: (root_types: RootType),
    // TODO: generics
} }

concatenation! { type_annotations: TypeAnnotation {
    > colon: Colon,
    ty: (types: Type),
} }

concatenation! { type_expressions: TypeExpression {
    // TODO: Type is both essential but also optional
    //       "Optional" is currently implemented with separate expression alternatives
    //       -> Make last_essential optional as well to simplify this use-case?
    > type_kw: Type,
    ty: (types: Type),
} }

concatenation! { type_groups: TypeGroup {
    > opening_paren: LParen,
    ty: (types: Type),
    closing_paren: RParen,
} }

token_alternation! { unary_operators: UnaryOperator {
    Neg: Minus,
    Not: Not,
} }

concatenation! { unit_shorthands: UnitShorthand {
    opening_paren: LParen,
    > closing_paren: RParen,
} }

concatenation! { while_blocks: WhileBlock {
    label: [Label],
    > while_kw: While,
    condition: (conditions: Condition),
    block: (blocks: Block),
} }
