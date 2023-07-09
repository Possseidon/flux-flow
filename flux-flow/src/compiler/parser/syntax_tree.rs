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
    ( $T:ident {
        $( $essential_fields:ident: $EssentialFields:tt, )*
        > $last_essential_field:ident: $LastEssentialField:tt,
        $( $required_fields:ident: $RequiredFields:tt, )*
    } ) => {
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
    ( $T:ident {
        $( $alternative:ident: $Alternative:tt, )*
    } ) => {
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
                    $( Self::$alternative(node_or_token) => {
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
    ( $T:ident { $field:ident: $Field:tt } ) => {
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
    ( $T:ident { $field:ident: $Field:tt } ) => {
        repetition_helper!($T Repetition $field $Field);
    };
    ( $T:ident { ($field:ident): $Field:tt } ) => {
        braced_repetition_helper!($T opening_paren closing_paren Paren $field $Field);
    };
    ( $T:ident { [$field:ident]: $Field:tt } ) => {
        braced_repetition_helper!($T opening_brack closing_brack Brack $field $Field);
    };
    ( $T:ident { {$field:ident}: $Field:tt } ) => {
        braced_repetition_helper!($T opening_curly closing_curly Curly $field $Field);
    };
}

macro_rules! syntax_tree_nodes {
    ( $( $T:ident: $macro:ident $body:tt )* ) => {
        paste! {
            #[derive(Debug, Default)]
            struct SyntaxTreeNodes {
                $( [<$T:snake _nodes>]: Vec<$T>, )*
            }
        }

        $( $macro!( $T $body ); )*
    };
}

syntax_tree_nodes! {

ArgumentExpression: alternation {
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
}

ArrayLiteral: repetition { [items]: (ref TrailingCommaExpression) }

ArrayUpdater: repetition { [assignments]: (ref KeyValue) }

BinaryOperation: concatenation {
    > operator: (ref BinaryOperator),
    expression: (ref SuffixExpression),
}

BinaryOperator: alternation {
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
}

Block: repetition { {statements}: (ref Statement) }

BreakExpression: concatenation {
    > break_kw: Break,
    label: [Label],
    expression: [ref Expression],
}

CollectionModifier: concatenation {
    > opening_brack: LBrack,
    kind: [ref CollectionModifierKind],
    closing_brack: RBrack,
}

CollectionModifierChain: repetition { modifiers: (ref CollectionModifier) }

CollectionModifierKind: alternation {
    Array: Integer,
    Deque: Tilde,
    Set: Lt,
    Map: (ref Type),
}

Condition: alternation {
    // TODO: Different from LetStatement
    // Let: (ref LetStatement),
    Expression: (ref Expression),
}

ContinueExpression: concatenation {
    > continue_kw: Continue,
    label: [Label],
}

ControlFlowBlock: alternation {
    Block: (ref Block),
    For: (ref ForBlock),
    Loop: (ref LoopBlock),
    While: (ref WhileBlock),
}

ElseBlock: concatenation {
    > else_kw: Else,
    block: (ref Block),
}

ElseIfBlock: concatenation {
    else_kw: Else,
    > if_kw: If,
    condition: (ref Condition),
    block: (ref Block),
}

ElseIfChain: repetition { else_if_blocks: (ref ElseIfBlock) }

Expression: concatenation {
    prefix: (ref PrefixOperationChain),
    > root: (ref RootExpression),
    suffix: (ref SuffixOperationChain),
}

ExpressionGroup: concatenation {
    > opening_paren: LParen,
    expression: (ref Expression),
    closing_paren: RParen,
}

ForBlock: concatenation {
    > for_kw: For,
    pattern: (ref Pattern),
    in_kw: In,
    expression: (ref Expression),
    block: (ref Block),
}

Function: concatenation {
    pub_kw: [Pub],
    > fn_kw: Fn,
    name: Ident,
    pattern: (ref Pattern),
    return_type: [ref ReturnType],
    block: (ref Block),
}

IfBlock: concatenation {
    > if_kw: If,
    condition: (ref Condition),
    block: (ref Block),
    else_if_chain: (ref ElseIfChain),
    else_block: [ref ElseBlock],
}

IndexExpression: alternation {
    Name: Ident,
    StructUpdate: (ref StructLiteral),
    ArrayUpdate: (ref ArrayUpdater),
    MapUpdate: (ref MapLiteral),
}

IndexOperation: concatenation {
    > operator: (ref IndexOperator),
    expression: (ref IndexExpression),
}

IndexOperator: alternation {
    Dot: Dot,
    DotAssign: DotEq,
}

Item: alternation {
    SubModule: (ref SubModule),
    Function: (ref Function),
}

KeyValue: concatenation {
    > key: (ref Expression),
    colon: Colon,
    value: (ref Expression),
    comma: [Comma],
}

Label: concatenation {
    > label: Label,
    colon: Colon,
}

LabeledControlFlowBlock: concatenation {
    // TODO: Optional essential would simplify this as well.
    > label: (ref Label),
    control_flow: (ref ControlFlowBlock),
}

LetStatement: concatenation {
    > let_kw: Let,
    name: Ident,
    type_annotation: [ref TypeAnnotation],
    eq: Eq,
    expression: (ref Expression),
}

LoopBlock: concatenation {
    > loop_kw: Loop,
    block: (ref Block),
}

MapEntryChain: repetition { {entries}: (ref KeyValue) }

MapLiteral: concatenation {
    at: At,
    > content: (ref MapEntryChain),
}

MatchArm: concatenation {
    > pattern: (ref Pattern),
    fat_arrow: FatArrow,
    expression: (ref Expression),
    comma: [Comma],
}

MatchBlock: concatenation {
    > match_kw: Match,
    expression: (ref Expression),
    block: (ref MatchBody),
}

MatchBody: repetition { {arms}: (ref MatchArm) }

Module: global_repetition { items: (ref Item) }

ModuleBlock: repetition { {items}: (ref Item) }

Path: concatenation {
    leading_path_sep: [PathSep],
    > name: Ident,
    chain: (ref PathResolutionChain),
}

PathResolution: concatenation {
    > path_sep: PathSep,
    name: Ident,
}

PathResolutionChain: repetition { chain: (ref PathResolution) }

Pattern: alternation {
    Discard: Underscore,
    VariablePattern: (ref VariablePattern),
    StructDestructure: Underscore, // TODO
    ArrayDestructure: Underscore, // TODO
}

PrefixOperation: alternation {
    UnaryOperator: (ref UnaryOperator),
}

PrefixOperationChain: repetition { operations: (ref PrefixOperation) }

ReturnExpression: concatenation {
    > return_kw: Return,
    expression: [ref Expression],
}

ReturnType: concatenation {
    > r_arrow: RArrow,
    ty: (ref Type),
}

RootExpression: alternation {
    LabeledControlFlow: (ref LabeledControlFlowBlock),
    ControlFlow: (ref ControlFlowBlock),
    Argument: (ref ArgumentExpression),
}

RootType: alternation {
    Deduce: Underscore,
    Never: Not,
    Path: (ref Path),
    Struct: (ref StructType),
    Group: (ref TypeGroup),
}

SetItemChain: repetition { {items}: (ref TrailingCommaExpression) }

SetLiteral: concatenation {
    > pound: Pound,
    content: (ref SetItemChain),
}

Statement: alternation {
    Semi: Semi,
    Let: (ref LetStatement),
    // If: (ref IfBlock),
    // Match: (ref MatchBlock),
    // LabeledControlFlow: (ref LabeledControlFlowBlock),
    // ControlFlow: (ref ControlFlowBlock),
    // Expression after blocks as they overlap.
    Expression: (ref Expression),
}

StructField: concatenation {
    name: [ref StructFieldName],
    > colon: Colon,
    expression: (ref Expression),
    comma: [Comma],
}

StructFieldName: concatenation {
    name: Ident,
    > colon: Colon,
}

StructFieldType: concatenation {
    name: (ref StructFieldName),
    > ty: (ref Type),
    comma: [Comma],
}

StructFieldTypeChain: repetition { (fields): (ref StructFieldType) }

StructLiteral: repetition { (fields): (ref StructField) }

StructType: concatenation {
    at: At,
    > types: (ref StructFieldTypeChain),
}

SubModule: concatenation {
    > mod_kw: Mod,
    name: Ident,
    block: (ref ModuleBlock),
}

SuffixExpression: concatenation {
    prefix: (ref PrefixOperationChain),
    > root: (ref RootExpression),
}

SuffixOperation: alternation {
    Binary: (ref BinaryOperation),
    Index: (ref IndexOperation),
    Call: (ref ArgumentExpression),
}

SuffixOperationChain: repetition { operations: (ref SuffixOperation) }

TrailingCommaExpression: concatenation {
    > expression: (ref Expression),
    comma: [Comma],
}

Type: concatenation {
    collection_modifiers: (ref CollectionModifierChain),
    > root: (ref RootType),
    // TODO: generics
    option: [Question],
}

TypeAnnotation: concatenation {
    > colon: Colon,
    ty: (ref Type),
}

TypeExpression: concatenation {
    // TODO: Type is both essential but also optional
    //       "Optional" is currently implemented with separate expression alternatives
    //       -> Make last_essential optional as well to simplify this use-case?
    //
    //       Update: Or should `ty` just be the last essential instead? Did this cause problems?
    > type_kw: Type,
    ty: (ref Type),
}

TypeGroup: concatenation {
    > opening_paren: LParen,
    ty: (ref Type),
    closing_paren: RParen,
}

UnaryOperator: alternation {
    Neg: Minus,
    Not: Not,
}

VariablePattern: concatenation {
    > name: Ident,
    type_annotation: [ref TypeAnnotation],
}

WhileBlock: concatenation {
    > while_kw: While,
    condition: (ref Condition),
    block: (ref Block),
}

}
