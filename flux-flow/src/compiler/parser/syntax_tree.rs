use std::{
    marker::PhantomData,
    num::NonZeroUsize,
    ops::{Index, IndexMut, Range},
    slice,
};

use lazy_static::lazy_static;
use paste::paste;

use crate::compiler::lexer::{BraceKind, TokenKind};

use super::{
    grammar::{Grammar, GrammarBuilder, OptionalRule, RecursiveRule, Rule},
    syntax_tree_node_builder::{Buildable, NodeBuilderError, NodeBuilderReader, UntypedNodeRef},
};

// TODO: Don't create node refs for empty repetitions.

#[derive(Debug)]
pub struct SyntaxTree {
    initial_whitespace: usize,
    nodes: SyntaxTreeNodes,
}

#[derive(Debug, Default)]
struct SyntaxTreeNodes {
    argument_expression_nodes: Vec<ArgumentExpression>,
    array_literal_nodes: Vec<ArrayLiteral>,
    array_updater_nodes: Vec<ArrayUpdater>,
    binary_operation_nodes: Vec<BinaryOperation>,
    binary_operator_nodes: Vec<BinaryOperator>,
    block_nodes: Vec<Block>,
    break_expression_nodes: Vec<BreakExpression>,
    collection_modifier_chain_nodes: Vec<CollectionModifierChain>,
    collection_modifier_kind_nodes: Vec<CollectionModifierKind>,
    collection_modifier_nodes: Vec<CollectionModifier>,
    condition_nodes: Vec<Condition>,
    continue_expression_nodes: Vec<ContinueExpression>,
    control_flow_block_nodes: Vec<ControlFlowBlock>,
    else_block_nodes: Vec<ElseBlock>,
    else_if_block_nodes: Vec<ElseIfBlock>,
    else_if_chain_nodes: Vec<ElseIfChain>,
    expression_group_nodes: Vec<ExpressionGroup>,
    expression_nodes: Vec<Expression>,
    for_block_nodes: Vec<ForBlock>,
    function_nodes: Vec<Function>,
    if_block_nodes: Vec<IfBlock>,
    index_expression_nodes: Vec<IndexExpression>,
    index_operation_nodes: Vec<IndexOperation>,
    index_operator_nodes: Vec<IndexOperator>,
    item_nodes: Vec<Item>,
    key_value_nodes: Vec<KeyValue>,
    label_nodes: Vec<Label>,
    labeled_control_flow_block_nodes: Vec<LabeledControlFlowBlock>,
    let_statement_nodes: Vec<LetStatement>,
    loop_block_nodes: Vec<LoopBlock>,
    map_entry_chain_nodes: Vec<MapEntryChain>,
    map_literal_nodes: Vec<MapLiteral>,
    match_arm_nodes: Vec<MatchArm>,
    match_block_nodes: Vec<MatchBlock>,
    match_body_nodes: Vec<MatchBody>,
    module_nodes: Vec<Module>,
    path_nodes: Vec<Path>,
    path_resolution_chain_nodes: Vec<PathResolutionChain>,
    path_resolution_nodes: Vec<PathResolution>,
    pattern_nodes: Vec<Pattern>,
    prefix_operation_chain_nodes: Vec<PrefixOperationChain>,
    prefix_operation_nodes: Vec<PrefixOperation>,
    return_expression_nodes: Vec<ReturnExpression>,
    return_type_nodes: Vec<ReturnType>,
    root_expression_nodes: Vec<RootExpression>,
    root_type_nodes: Vec<RootType>,
    set_item_chain_nodes: Vec<SetItemChain>,
    set_literal_nodes: Vec<SetLiteral>,
    statement_nodes: Vec<Statement>,
    struct_field_name_nodes: Vec<StructFieldName>,
    struct_field_nodes: Vec<StructField>,
    struct_field_type_chain_nodes: Vec<StructFieldTypeChain>,
    struct_field_type_nodes: Vec<StructFieldType>,
    struct_literal_nodes: Vec<StructLiteral>,
    struct_type_nodes: Vec<StructType>,
    suffix_expression_nodes: Vec<SuffixExpression>,
    suffix_operation_chain_nodes: Vec<SuffixOperationChain>,
    suffix_operation_nodes: Vec<SuffixOperation>,
    trailing_comma_expression_nodes: Vec<TrailingCommaExpression>,
    type_annotation_nodes: Vec<TypeAnnotation>,
    type_expression_nodes: Vec<TypeExpression>,
    type_group_nodes: Vec<TypeGroup>,
    type_nodes: Vec<Type>,
    unary_operator_nodes: Vec<UnaryOperator>,
    variable_pattern_nodes: Vec<VariablePattern>,
    while_block_nodes: Vec<WhileBlock>,
}

impl SyntaxTree {
    pub fn new(initial_whitespace: usize) -> Self {
        Self {
            initial_whitespace,
            nodes: SyntaxTreeNodes::default(),
        }
    }

    pub fn map_nodes<'a, I: 'a, T>(&'a self, f: impl FnMut(&'a I) -> T) -> NodeMap<T, I>
    where
        Self: NodeIterator<I>,
    {
        NodeMap::new(self, f)
    }

    pub fn default_node_map<T: Default, I>(&self) -> NodeMap<T, I>
    where
        Self: NodeIterator<I>,
    {
        NodeMap::default(self)
    }

    pub fn initial_whitespace(&self) -> usize {
        self.initial_whitespace
    }

    pub fn root_module(&self) -> &Module {
        self.nodes
            .module_nodes
            .last()
            .expect("root module should exist")
    }

    pub fn visualize(&self, code: &str) {
        self.root_module().visualize(self, code, 0, false);
    }
}

pub struct NodeRef<T>(usize, PhantomData<*const T>);

pub trait NodeIterator<T> {
    fn iter(&self) -> slice::Iter<T>;
    fn iter_mut(&mut self) -> slice::IterMut<T>;
}

impl<T> Clone for NodeRef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for NodeRef<T> {}

impl<T> std::fmt::Debug for NodeRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("NodeRef").field(&self.0).finish()
    }
}

impl<T> PartialEq for NodeRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for NodeRef<T> {}

impl<T> NodeRef<T> {
    pub fn new(index: usize) -> Self {
        Self(index, Default::default())
    }
}

pub struct NodeMap<T, I>(Vec<T>, PhantomData<*const I>);

impl<T, I> IntoIterator for NodeMap<T, I> {
    type Item = T;
    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T, I> Index<NodeRef<I>> for NodeMap<T, I> {
    type Output = T;

    fn index(&self, index: NodeRef<I>) -> &Self::Output {
        &self.0[index.0]
    }
}

impl<T, I> IndexMut<NodeRef<I>> for NodeMap<T, I> {
    fn index_mut(&mut self, index: NodeRef<I>) -> &mut Self::Output {
        &mut self.0[index.0]
    }
}

impl<T: Clone, I> Clone for NodeMap<T, I> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), Default::default())
    }
}

impl<T: std::fmt::Debug, I> std::fmt::Debug for NodeMap<T, I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("NodeMap").field(&self.0).finish()
    }
}

impl<T, I> Default for NodeMap<T, I> {
    fn default() -> Self {
        Self(Default::default(), Default::default())
    }
}

impl<T: PartialEq, I> PartialEq for NodeMap<T, I> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: Eq, I> Eq for NodeMap<T, I> {}

impl<T, I> NodeMap<T, I>
where
    SyntaxTree: NodeIterator<I>,
{
    pub fn new<'a>(syntax_tree: &'a SyntaxTree, f: impl FnMut(&'a I) -> T) -> NodeMap<T, I>
    where
        I: 'a,
    {
        Self(syntax_tree.iter().map(f).collect(), Default::default())
    }

    pub fn default(syntax_tree: &SyntaxTree) -> Self
    where
        T: Default,
    {
        Self::new(syntax_tree, |_| Default::default())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> slice::Iter<T> {
        self.0.iter()
    }

    pub fn iter_mut(&mut self) -> slice::IterMut<T> {
        self.0.iter_mut()
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
    ( $grammar:ident ( ref $T:ident ) ) => {
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
    ( $grammar:ident ( ref $T:ident ) ) => {
        OptionalRule::Required(Rule::Ref($grammar.rule::<$T>()))
    };
    ( $grammar:ident [ ref $T:ident ] ) => {
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
    ( ( ref $T:ident ) ) => {
        NodeRef<$T>
    };
    ( [ ref $T:ident ] ) => {
        Option<NodeRef<$T>>
    };
}

macro_rules! visualize {
    ( $syntax_tree:ident $code:ident $indent:tt $with_indent:tt $field:tt $rule:ident ) => {
        $field.visualize($syntax_tree, $code, $indent, $with_indent);
    };
    ( $syntax_tree:ident $code:ident $indent:tt $with_indent:tt $field:tt [ $rule:ident ] ) => {
        $field.visualize($syntax_tree, $code, $indent, $with_indent);
    };
    ( $syntax_tree:ident $code:ident $indent:tt $with_indent:tt $field:tt ( ref $T:ident ) ) => {
        paste! {
            $syntax_tree.nodes.[<$T:snake _nodes>][$field.0].visualize($syntax_tree, $code, $indent, $with_indent);
        }
    };
    ( $syntax_tree:ident $code:ident $indent:tt $with_indent:tt $field:tt [ ref $T:ident ] ) => {
        if let Some(NodeRef(i, _)) = $field {
            paste! {
                $syntax_tree.nodes.[<$T:snake _nodes>][i].visualize($syntax_tree, $code, $indent, $with_indent);
            }
        } else {
            println!("n/a");
        }
    };
}

macro_rules! impl_node_ref_helpers {
    ( $T:ident ) => {
        paste! {
            impl Index<NodeRef<$T>> for SyntaxTree {
                type Output = $T;

                fn index(&self, index: NodeRef<$T>) -> &Self::Output {
                    &self.nodes.[<$T:snake _nodes>][index.0]
                }
            }

            impl IndexMut<NodeRef<$T>> for SyntaxTree {
                fn index_mut(&mut self, index: NodeRef<$T>) -> &mut Self::Output {
                    &mut self.nodes.[<$T:snake _nodes>][index.0]
                }
            }

            impl NodeIterator<$T> for SyntaxTree {
                fn iter(&self) -> slice::Iter<$T> {
                    self.nodes.[<$T:snake _nodes>].iter()
                }

                fn iter_mut(&mut self) -> slice::IterMut<$T> {
                    self.nodes.[<$T:snake _nodes>].iter_mut()
                }
            }
        }
    };
}

macro_rules! concatenation {
    { $T:ident {
        $( $essential_fields:ident: $EssentialFields:tt, )*
        > $last_essential_field:ident: $LastEssentialField:tt,
        $( $required_fields:ident: $RequiredFields:tt, )*
    } } => {
        #[derive(Clone, Copy, Debug)]
        pub struct $T {
            $( pub $essential_fields: field_type!($EssentialFields), )*
            pub $last_essential_field: field_type!($LastEssentialField),
            $( pub $required_fields: field_type!($RequiredFields), )*
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

        impl_node_ref_helpers!($T);

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

            paste! {
                fn build(
                    syntax_tree: &mut SyntaxTree,
                    reader: &mut NodeBuilderReader,
                ) -> Result<Option<UntypedNodeRef>, NodeBuilderError> {
                    let node_ref = UntypedNodeRef(syntax_tree.nodes.[<$T:snake _nodes>].len());
                    syntax_tree.nodes.[<$T:snake _nodes>].push(Self {
                        $( $essential_fields: reader.read()?, )*
                        $last_essential_field: reader.read()?,
                        $( $required_fields: reader.read()?, )*
                    });
                    Ok(Some(node_ref))
                }
            }
        }
    };
}

macro_rules! alternation {
    { $T:ident {
        $( $alternative:ident: $Alternative:tt, )*
    } } => {
        #[derive(Clone, Copy, Debug)]
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
                print!("{}::", <Self as Buildable>::name());
                match &self {
                    $( $T::$alternative(node_or_token) => {
                        print!("{} > ", stringify!($alternative));
                        visualize!(syntax_tree code indent false node_or_token $Alternative);
                    } )*
                }
            }
        }

        impl_node_ref_helpers!($T);

        impl Buildable for $T {
            fn name() -> &'static str {
                stringify!($T)
            }

            fn rule(#[allow(unused_variables)] grammar: &mut GrammarBuilder) -> RecursiveRule {
                RecursiveRule::Alternation(vec![
                    $( rule!(grammar $Alternative), )*
                ])
            }

            paste! {
                fn build(
                    syntax_tree: &mut SyntaxTree,
                    reader: &mut NodeBuilderReader,
                ) -> Result<Option<UntypedNodeRef>, NodeBuilderError> {
                    let node_ref = UntypedNodeRef(syntax_tree.nodes.[<$T:snake _nodes>].len());
                    $(
                        if let Some(node) = reader.read()? {
                            syntax_tree.nodes.[<$T:snake _nodes>].push($T::$alternative(node));
                            return Ok(Some(node_ref));
                        }
                    )*
                    Ok(None)
                }
            }
        }
    };
}

macro_rules! token_alternation {
    { $T:ident {
        $( $alternative:ident $( $alternative_node:ident )?: $Alternative:tt, )*
    } } => {
        #[derive(Clone, Copy, Debug)]
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

        impl_node_ref_helpers!($T);

        impl Buildable for $T {
            fn name() -> &'static str {
                stringify!($T)
            }

            fn rule(#[allow(unused_variables)] grammar: &mut GrammarBuilder) -> RecursiveRule {
                RecursiveRule::Alternation(vec![
                    $( rule!(grammar $Alternative), )*
                ])
            }

            paste!{
                fn build(
                    syntax_tree: &mut SyntaxTree,
                    reader: &mut NodeBuilderReader,
                ) -> Result<Option<UntypedNodeRef>, NodeBuilderError> {
                    let node_ref = UntypedNodeRef(syntax_tree.nodes.[<$T:snake _nodes>].len());
                    $(
                        if let Some(node) = reader.read()? {
                            syntax_tree.nodes.[<$T:snake _nodes>].push(Self::$alternative(node));
                            return Ok(Some(node_ref));
                        }
                    )*
                    Ok(None)
                }
            }
        }
    };
}

macro_rules! repetition_field_type {
    ( $rule:ident ) => {
        Vec<Token>
    };
    ( ( ref $T:ident ) ) => {
        Vec<NodeRef<$T>>
    };
}

macro_rules! repetition_helper {
    ( $T:ident $repetition_kind:ident $field:ident $Field:tt ) => {
        #[derive(Clone, Debug)]
        pub struct $T {
            pub $field: repetition_field_type!($Field),
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
                print!("{}", <Self as Buildable>::name());
                println!("[{}]", self.$field.len());
                for field in &self.$field {
                    visualize!(syntax_tree code (indent + 1) true field $Field);
                }
            }
        }

        impl_node_ref_helpers!($T);

        impl Buildable for $T {
            fn name() -> &'static str {
                stringify!($T)
            }

            fn rule(#[allow(unused_variables)] grammar: &mut GrammarBuilder) -> RecursiveRule {
                RecursiveRule::$repetition_kind(rule!(grammar $Field))
            }

            paste! {
                fn build(
                    syntax_tree: &mut SyntaxTree,
                    reader: &mut NodeBuilderReader,
                ) -> Result<Option<UntypedNodeRef>, NodeBuilderError> {
                    let node_ref = UntypedNodeRef(syntax_tree.nodes.[<$T:snake _nodes>].len());
                    syntax_tree.nodes.[<$T:snake _nodes>].push(Self {
                        $field: reader.read()?,
                    });
                    Ok(Some(node_ref))
                }
            }
        }
    };
}

macro_rules! global_repetition {
    ( $T:ident => $field:ident: $Field:tt ) => {
        repetition_helper!($T GlobalRepetition $field $Field);
    };
}

macro_rules! braced_repetition_helper {
    ( $T:ident $opening:ident $closing:ident $brace_kind:ident $field:ident $Field:tt ) => {
        paste! {
            #[derive(Clone, Debug)]
            pub struct $T {
                pub $opening: Token,
                pub $field: repetition_field_type!($Field),
                pub $closing: Token,
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
                    print!("{}", <Self as Buildable>::name());
                    println!("[{}]", self.$field.len());
                    for field in &self.$field {
                        visualize!(syntax_tree code (indent + 1) true field $Field);
                    }
                }
            }

            impl_node_ref_helpers!($T);

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
                ) -> Result<Option<UntypedNodeRef>, NodeBuilderError> {
                    let node_ref = UntypedNodeRef(syntax_tree.nodes.[<$T:snake _nodes>].len());
                    syntax_tree.nodes.[<$T:snake _nodes>].push(Self {
                        $opening: reader.read()?,
                        $field: reader.read()?,
                        $closing: reader.read()?,
                    });
                    Ok(Some(node_ref))
                }
            }
        }
    };
}

macro_rules! repetition {
    ( $T:ident => $field:ident: $Field:tt ) => {
        repetition_helper!($T Repetition $field $Field);
    };
    ( $T:ident => ( $field:ident: $Field:tt ) ) => {
        braced_repetition_helper!($T opening_paren closing_paren Paren $field $Field);
    };
    ( $T:ident => [ $field:ident: $Field:tt ] ) => {
        braced_repetition_helper!($T opening_brack closing_brack Brack $field $Field);
    };
    ( $T:ident => { $field:ident: $Field:tt } ) => {
        braced_repetition_helper!($T opening_curly closing_curly Curly $field $Field);
    };
}

alternation! { ArgumentExpression {
    False: False,
    True: True,
    Integer: Integer,
    Float: Float,
    String: String,
    Char: Char,
    Path: (ref Path),
    Struct: (ref StructLiteral),
    Array: (ref ArrayLiteral),
    Set: (ref SetLiteral),
    Map: (ref MapLiteral),
    Return: (ref ReturnExpression),
    Break: (ref BreakExpression),
    Continue: (ref ContinueExpression),
    Group: (ref ExpressionGroup),
    If: (ref IfBlock),
    Match: (ref MatchBlock),
    TypeWithPrefix: (ref TypeExpression),
    // Type last, as it has overlap with literals which must be resolved using the `type` prefix.
    Type: (ref Type),
} }

repetition!(ArrayLiteral => [ items: (ref TrailingCommaExpression) ]);

repetition!(ArrayUpdater => [ assignments: (ref KeyValue) ]);

concatenation! { BinaryOperation {
    > operator: (ref BinaryOperator),
    expression: (ref SuffixExpression),
} }

token_alternation! { BinaryOperator {
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

repetition!(Block => { statements: (ref Statement) });

concatenation! { BreakExpression {
    > break_kw: Break,
    label: [Label],
    expression: [ref Expression],
} }

concatenation! { CollectionModifier {
    > opening_brack: LBrack,
    kind: [ref CollectionModifierKind],
    closing_brack: RBrack,
}}

repetition!(CollectionModifierChain => modifiers: (ref CollectionModifier));

alternation! { CollectionModifierKind {
    Array: Integer,
    Deque: Tilde,
    Set: Lt,
    Map: (ref Type),
} }

alternation! { Condition {
    // TODO: Different from LetStatement
    // Let: (ref LetStatement),
    Expression: (ref Expression),
} }

concatenation! { ContinueExpression {
    > continue_kw: Continue,
    label: [Label],
} }

alternation! { ControlFlowBlock {
    Block: (ref Block),
    For: (ref ForBlock),
    Loop: (ref LoopBlock),
    While: (ref WhileBlock),
} }

concatenation! { ElseBlock {
    > else_kw: Else,
    block: (ref Block),
} }

concatenation! { ElseIfBlock {
    else_kw: Else,
    > if_kw: If,
    condition: (ref Condition),
    block: (ref Block),
} }

repetition!(ElseIfChain => else_if_blocks: (ref ElseIfBlock));

concatenation! { Expression {
    prefix: (ref PrefixOperationChain),
    > root: (ref RootExpression),
    suffix: (ref SuffixOperationChain),
} }

concatenation! { ExpressionGroup {
    > opening_paren: LParen,
    expression: (ref Expression),
    closing_paren: RParen,
} }

concatenation! { ForBlock {
    > for_kw: For,
    pattern: (ref Pattern),
    in_kw: In,
    expression: (ref Expression),
    block: (ref Block),
} }

concatenation! { Function {
    pub_kw: [Pub],
    > fn_kw: Fn,
    name: Ident,
    pattern: (ref Pattern),
    return_type: [ref ReturnType],
    block: (ref Block),
} }

concatenation! { IfBlock {
    > if_kw: If,
    condition: (ref Condition),
    block: (ref Block),
    else_if_chain: (ref ElseIfChain),
    else_block: [ref ElseBlock],
} }

alternation! { IndexExpression {
    Name: Ident,
    StructUpdate: (ref StructLiteral),
    ArrayUpdate: (ref ArrayUpdater),
    MapUpdate: (ref MapLiteral),
} }

concatenation! { IndexOperation {
    > operator: (ref IndexOperator),
    expression: (ref IndexExpression),
} }

token_alternation! { IndexOperator {
    Dot: Dot,
    DotAssign: DotEq,
} }

alternation! { Item {
    Function: (ref Function),
} }

concatenation! { KeyValue {
    > key: (ref Expression),
    colon: Colon,
    value: (ref Expression),
    comma: [Comma],
} }

concatenation! { Label {
    > label: Label,
    colon: Colon,
} }

concatenation! { LabeledControlFlowBlock {
    // TODO: Optional essential would simplify this as well.
    > label: (ref Label),
    control_flow: (ref ControlFlowBlock),
} }

concatenation! { LetStatement {
    > let_kw: Let,
    name: Ident,
    type_annotation: [ref TypeAnnotation],
    eq: Eq,
    expression: (ref Expression),
} }

concatenation! { LoopBlock {
    > loop_kw: Loop,
    block: (ref Block),
} }

repetition!(MapEntryChain => { entries: (ref KeyValue) });

concatenation! { MapLiteral {
    at: At,
    > content: (ref MapEntryChain),
} }

concatenation! { MatchArm {
    > pattern: (ref Pattern),
    fat_arrow: FatArrow,
    expression: (ref Expression),
    comma: [Comma],
} }

concatenation! { MatchBlock {
    > match_kw: Match,
    expression: (ref Expression),
    block: (ref MatchBody),
} }

repetition!(MatchBody => { arms: (ref MatchArm) });

global_repetition!(Module => items: (ref Item));

concatenation! { Path {
    leading_path_sep: [PathSep],
    > name: Ident,
    chain: (ref PathResolutionChain),
} }

concatenation! { PathResolution {
    > path_sep: PathSep,
    name: Ident,
} }

repetition!(PathResolutionChain => chain: (ref PathResolution));

alternation! { Pattern {
    Discard: Underscore,
    VariablePattern: (ref VariablePattern),
    StructDestructure: Underscore, // TODO
    ArrayDestructure: Underscore, // TODO
} }

alternation! { PrefixOperation {
    UnaryOperator: (ref UnaryOperator),
} }

repetition!(PrefixOperationChain => operations: (ref PrefixOperation));

concatenation! { ReturnExpression {
    > return_kw: Return,
    expression: [ref Expression],
} }

concatenation! { ReturnType {
    > r_arrow: RArrow,
    ty: (ref Type),
} }

alternation! { RootExpression {
    LabeledControlFlow: (ref LabeledControlFlowBlock),
    ControlFlow: (ref ControlFlowBlock),
    Argument: (ref ArgumentExpression),
} }

alternation! { RootType {
    Deduce: Underscore,
    Never: Not,
    Path: (ref Path),
    Struct: (ref StructType),
    Group: (ref TypeGroup),
} }

repetition!(SetItemChain => { items: (ref TrailingCommaExpression) });

concatenation! { SetLiteral {
    > pound: Pound,
    content: (ref SetItemChain),
} }

alternation! { Statement {
    Semi: Semi,
    Let: (ref LetStatement),
    // If: (ref IfBlock),
    // Match: (ref MatchBlock),
    // LabeledControlFlow: (ref LabeledControlFlowBlock),
    // ControlFlow: (ref ControlFlowBlock),
    // Expression after blocks as they overlap.
    Expression: (ref Expression),
} }

concatenation! { StructField {
    name: [ref StructFieldName],
    > colon: Colon,
    expression: (ref Expression),
    comma: [Comma],
} }

concatenation! { StructFieldName {
    name: Ident,
    > colon: Colon,
} }

concatenation! { StructFieldType {
    name: (ref StructFieldName),
    > ty: (ref Type),
    comma: [Comma],
} }

repetition!(StructFieldTypeChain => ( fields: (ref StructFieldType) ));

repetition!(StructLiteral => ( fields: (ref StructField) ));

concatenation! { StructType {
    at: At,
    > types: (ref StructFieldTypeChain),
} }

concatenation! { SuffixExpression {
    prefix: (ref PrefixOperationChain),
    > root: (ref RootExpression),
} }

alternation! { SuffixOperation {
    Binary: (ref BinaryOperation),
    Index: (ref IndexOperation),
    Call: (ref ArgumentExpression),
} }

repetition!(SuffixOperationChain => operations: (ref SuffixOperation));

concatenation! { TrailingCommaExpression {
    > expression: (ref Expression),
    comma: [Comma],
} }

concatenation! { Type {
    collection_modifiers: (ref CollectionModifierChain),
    > root: (ref RootType),
    // TODO: generics
    option: [Question],
} }

concatenation! { TypeAnnotation {
    > colon: Colon,
    ty: (ref Type),
} }

concatenation! { TypeExpression {
    // TODO: Type is both essential but also optional
    //       "Optional" is currently implemented with separate expression alternatives
    //       -> Make last_essential optional as well to simplify this use-case?
    //
    //       Update: Or should `ty` just be the last essential instead? Did this cause problems?
    > type_kw: Type,
    ty: (ref Type),
} }

concatenation! { TypeGroup {
    > opening_paren: LParen,
    ty: (ref Type),
    closing_paren: RParen,
} }

token_alternation! { UnaryOperator {
    Neg: Minus,
    Not: Not,
} }

concatenation! { VariablePattern {
    > name: Ident,
    type_annotation: [ref TypeAnnotation],
} }

concatenation! { WhileBlock {
    > while_kw: While,
    condition: (ref Condition),
    block: (ref Block),
} }
