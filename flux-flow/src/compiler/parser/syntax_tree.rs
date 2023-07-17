use std::{
    marker::PhantomData,
    num::NonZeroUsize,
    ops::{Index, IndexMut, Range},
    slice,
};

use lazy_static::lazy_static;
use paste::paste;

use crate::compiler::lexer::{GrammarToken, GroupKind, KeywordToken, PunctuationToken};

use super::{
    grammar::{
        AlternationRule, EssentialRule, EssentialRuleMode, GlobalRule, Grammar, GrammarBuilder,
        GroupRule, LastEssentialRule, RecursiveRule, RequiredRule, RequiredRuleMode, Rule,
    },
    node_builder::{
        Buildable, NodeBuilderContent, NodeBuilderReadable, NodeBuilderReader, UntypedNodeRef,
    },
};

trait Visualize {
    fn visualize(&self, syntax_tree: &SyntaxTree, code: &str, indent: usize, with_indent: bool);
}

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

pub trait NodeIterator<T> {
    fn iter(&self) -> slice::Iter<T>;
    fn iter_mut(&mut self) -> slice::IterMut<T>;
}

pub struct CodeSpanEnd {
    pub index: usize,
    pub index_with_trailing_whitespace: usize,
}

pub trait CodeSpan {
    /// Return the index of the first token or a [`CodeSpan`] to recurse into.
    ///
    /// Usually you just want to call [`CodeSpan::span_start()`] which automatically does the
    /// recursion for you in a tail-call optimized manner to avoid a stack overflow.
    fn recurse_span_start<'a>(
        &self,
        syntax_tree: &'a SyntaxTree,
    ) -> Result<usize, &'a dyn CodeSpan>;

    /// Returns the end index of the last token or a [`CodeSpan`] to recurse into.
    ///
    /// Usually you just want to call [`CodeSpan::span_end()`] which automatically does the
    /// recursion for you in a tail-call optimized manner to avoid a stack overflow.
    fn recurse_span_end<'a>(
        &self,
        syntax_tree: &'a SyntaxTree,
    ) -> Result<CodeSpanEnd, &'a dyn CodeSpan>;

    /// Recurses into [`CodeSpan::recurse_span_start()`] to get the index of the first token.
    ///
    /// Uses manually implemented tail-call recursion to avoid stack overflow.
    fn span_start(&self, syntax_tree: &SyntaxTree) -> usize
    where
        Self: Sized,
    {
        let mut current: &dyn CodeSpan = self;
        loop {
            match current.recurse_span_start(syntax_tree) {
                Ok(index) => break index,
                Err(span) => current = span,
            }
        }
    }

    /// Recurses into [`CodeSpan::recurse_span_end()`] to get the end index of the last token.
    ///
    /// Uses manually implemented tail-call recursion to avoid stack overflow.
    fn span_end(&self, syntax_tree: &SyntaxTree) -> CodeSpanEnd
    where
        Self: Sized,
    {
        let mut current: &dyn CodeSpan = self;
        loop {
            match current.recurse_span_end(syntax_tree) {
                Ok(indices) => break indices,
                Err(span) => current = span,
            }
        }
    }

    fn span(&self, syntax_tree: &SyntaxTree) -> Range<usize>
    where
        Self: Sized,
    {
        self.span_start(syntax_tree)..self.span_end(syntax_tree).index
    }

    fn span_with_trailing_whitespace(&self, syntax_tree: &SyntaxTree) -> Range<usize>
    where
        Self: Sized,
    {
        self.span_start(syntax_tree)..self.span_end(syntax_tree).index_with_trailing_whitespace
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token {
    pub code_index: usize,
    pub len: NonZeroUsize,
    pub trailing_whitespace_len: usize,
}

impl Token {
    pub fn token_span(self) -> Range<usize> {
        self.code_index..self.code_index + self.len.get()
    }
}

impl CodeSpan for Token {
    fn recurse_span_start<'a>(&self, _: &'a SyntaxTree) -> Result<usize, &'a dyn CodeSpan> {
        Ok(self.code_index)
    }

    fn recurse_span_end<'a>(&self, _: &'a SyntaxTree) -> Result<CodeSpanEnd, &'a dyn CodeSpan> {
        let index = self.code_index + self.len.get();
        Ok(CodeSpanEnd {
            index,
            index_with_trailing_whitespace: index + self.trailing_whitespace_len,
        })
    }
}

impl NodeBuilderReadable for Token {
    fn read(context: &mut NodeBuilderReader) -> Self {
        match context.read_content() {
            NodeBuilderContent::Token { token } => token,
            NodeBuilderContent::Node { .. }
            | NodeBuilderContent::OptionalMismatch { .. }
            | NodeBuilderContent::EndRepetition { .. } => panic!("token expected"),
        }
    }
}

impl Visualize for Token {
    fn visualize(&self, syntax_tree: &SyntaxTree, code: &str, indent: usize, with_indent: bool) {
        println!(
            "{: <2$}`{}`",
            "",
            &code[self.span(syntax_tree)],
            if with_indent { indent } else { 0 }
        );
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OptionalToken {
    Some(Token),
    None { code_index: usize },
}

impl CodeSpan for OptionalToken {
    fn recurse_span_start<'a>(
        &self,
        syntax_tree: &'a SyntaxTree,
    ) -> Result<usize, &'a dyn CodeSpan> {
        match self {
            OptionalToken::Some(token) => token.recurse_span_start(syntax_tree),
            OptionalToken::None { code_index } => Ok(*code_index),
        }
    }

    fn recurse_span_end<'a>(
        &self,
        syntax_tree: &'a SyntaxTree,
    ) -> Result<CodeSpanEnd, &'a dyn CodeSpan> {
        match self {
            OptionalToken::Some(token) => token.recurse_span_end(syntax_tree),
            OptionalToken::None { code_index } => Ok(CodeSpanEnd {
                index: *code_index,
                index_with_trailing_whitespace: *code_index,
            }),
        }
    }
}

impl NodeBuilderReadable for OptionalToken {
    fn read(context: &mut NodeBuilderReader) -> Self {
        match context.read_content() {
            NodeBuilderContent::Token { token } => OptionalToken::Some(token),
            NodeBuilderContent::OptionalMismatch { code_index } => {
                OptionalToken::None { code_index }
            }
            NodeBuilderContent::Node { .. } | NodeBuilderContent::EndRepetition { .. } => {
                panic!("optional token expected")
            }
        }
    }
}

impl Visualize for OptionalToken {
    fn visualize(&self, syntax_tree: &SyntaxTree, code: &str, indent: usize, with_indent: bool) {
        match self {
            OptionalToken::Some(token) => token.visualize(syntax_tree, code, indent, with_indent),
            OptionalToken::None { .. } => println!("n/a"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RepeatingToken {
    Some(Vec<Token>),
    None { code_index: usize },
}

impl CodeSpan for RepeatingToken {
    fn recurse_span_start<'a>(
        &self,
        syntax_tree: &'a SyntaxTree,
    ) -> Result<usize, &'a dyn CodeSpan> {
        match self {
            RepeatingToken::Some(tokens) => tokens
                .first()
                .expect("repeating token should have at least one token")
                .recurse_span_start(syntax_tree),
            RepeatingToken::None { code_index } => Ok(*code_index),
        }
    }

    fn recurse_span_end<'a>(
        &self,
        syntax_tree: &'a SyntaxTree,
    ) -> Result<CodeSpanEnd, &'a dyn CodeSpan> {
        match self {
            RepeatingToken::Some(tokens) => tokens
                .last()
                .expect("repeating token should have at least one token")
                .recurse_span_end(syntax_tree),
            RepeatingToken::None { code_index } => Ok(CodeSpanEnd {
                index: *code_index,
                index_with_trailing_whitespace: *code_index,
            }),
        }
    }
}

impl NodeBuilderReadable for RepeatingToken {
    fn read(context: &mut NodeBuilderReader) -> Self {
        let mut tokens = Vec::new();
        loop {
            match context.read_content() {
                NodeBuilderContent::Token { token } => tokens.push(token),
                NodeBuilderContent::EndRepetition { code_index } => {
                    break if tokens.is_empty() {
                        Self::None { code_index }
                    } else {
                        Self::Some(tokens)
                    }
                }
                NodeBuilderContent::Node { .. } | NodeBuilderContent::OptionalMismatch { .. } => {
                    panic!("repeating node expected")
                }
            }
        }
    }
}

impl Visualize for RepeatingToken {
    fn visualize(&self, syntax_tree: &SyntaxTree, code: &str, indent: usize, with_indent: bool) {
        if with_indent {
            print_indent(indent);
        }
        if let RepeatingToken::Some(tokens) = self {
            println!("[{}]", tokens.len());
            for token in tokens {
                token.visualize(syntax_tree, code, indent + 1, true);
            }
        } else {
            println!("[0]");
        }
    }
}

pub struct NodeRef<T>(usize, PhantomData<*const T>);

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
    pub fn new(node_ref: usize) -> Self {
        Self(node_ref, Default::default())
    }
}

impl<T> CodeSpan for NodeRef<T>
where
    SyntaxTree: Index<NodeRef<T>>,
    for<'a> <SyntaxTree as Index<NodeRef<T>>>::Output: CodeSpan + Sized + 'a,
{
    fn recurse_span_start<'a>(
        &self,
        syntax_tree: &'a SyntaxTree,
    ) -> Result<usize, &'a dyn CodeSpan> {
        Err(&syntax_tree[*self])
    }

    fn recurse_span_end<'a>(
        &self,
        syntax_tree: &'a SyntaxTree,
    ) -> Result<CodeSpanEnd, &'a dyn CodeSpan> {
        Err(&syntax_tree[*self])
    }
}

impl<T> NodeBuilderReadable for NodeRef<T> {
    fn read(context: &mut NodeBuilderReader) -> Self {
        match context.read_content() {
            NodeBuilderContent::Node { node } => NodeRef::new(node.0),
            _ => panic!("node expected"),
        }
    }
}

impl<T: Visualize> Visualize for NodeRef<T>
where
    SyntaxTree: Index<NodeRef<T>>,
    <SyntaxTree as Index<NodeRef<T>>>::Output: Visualize,
{
    fn visualize(&self, syntax_tree: &SyntaxTree, code: &str, indent: usize, with_indent: bool) {
        syntax_tree[*self].visualize(syntax_tree, code, indent, with_indent);
    }
}

pub enum OptionalNodeRef<T> {
    Some(NodeRef<T>),
    None { code_index: usize },
}

impl<T> Clone for OptionalNodeRef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for OptionalNodeRef<T> {}

impl<T> std::fmt::Debug for OptionalNodeRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Some(arg0) => f.debug_tuple("Some").field(arg0).finish(),
            Self::None { code_index } => f
                .debug_struct("None")
                .field("code_index", code_index)
                .finish(),
        }
    }
}

impl<T> PartialEq for OptionalNodeRef<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Some(l_node), Self::Some(r_node)) => l_node == r_node,
            (
                Self::None {
                    code_index: l_code_index,
                },
                Self::None {
                    code_index: r_code_index,
                },
            ) => l_code_index == r_code_index,
            _ => false,
        }
    }
}

impl<T> Eq for OptionalNodeRef<T> {}

impl<T> CodeSpan for OptionalNodeRef<T>
where
    NodeRef<T>: CodeSpan,
{
    fn recurse_span_start<'a>(
        &self,
        syntax_tree: &'a SyntaxTree,
    ) -> Result<usize, &'a dyn CodeSpan> {
        match self {
            OptionalNodeRef::Some(node) => node.recurse_span_start(syntax_tree),
            OptionalNodeRef::None { code_index } => Ok(*code_index),
        }
    }

    fn recurse_span_end<'a>(
        &self,
        syntax_tree: &'a SyntaxTree,
    ) -> Result<CodeSpanEnd, &'a dyn CodeSpan> {
        match self {
            OptionalNodeRef::Some(node) => node.recurse_span_end(syntax_tree),
            OptionalNodeRef::None { code_index } => Ok(CodeSpanEnd {
                index: *code_index,
                index_with_trailing_whitespace: *code_index,
            }),
        }
    }
}

impl<T> NodeBuilderReadable for OptionalNodeRef<T> {
    fn read(context: &mut NodeBuilderReader) -> Self {
        match context.read_content() {
            NodeBuilderContent::Node { node } => OptionalNodeRef::Some(NodeRef::new(node.0)),
            NodeBuilderContent::OptionalMismatch { code_index } => {
                OptionalNodeRef::None { code_index }
            }
            _ => panic!("optional node expected"),
        }
    }
}

impl<T: Visualize> Visualize for OptionalNodeRef<T>
where
    NodeRef<T>: Visualize,
{
    fn visualize(&self, syntax_tree: &SyntaxTree, code: &str, indent: usize, with_indent: bool) {
        match self {
            OptionalNodeRef::Some(node) => node.visualize(syntax_tree, code, indent, with_indent),
            OptionalNodeRef::None { .. } => println!("n/a"),
        }
    }
}

pub enum RepeatingNodeRef<T> {
    Some(Vec<NodeRef<T>>),
    None { code_index: usize },
}

impl<T> Clone for RepeatingNodeRef<T> {
    fn clone(&self) -> Self {
        match self {
            Self::Some(nodes) => Self::Some(nodes.clone()),
            Self::None { code_index } => Self::None {
                code_index: *code_index,
            },
        }
    }
}

impl<T> std::fmt::Debug for RepeatingNodeRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Some(nodes) => f.debug_tuple("Some").field(nodes).finish(),
            Self::None { code_index } => f
                .debug_struct("None")
                .field("code_index", code_index)
                .finish(),
        }
    }
}

impl<T> PartialEq for RepeatingNodeRef<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Some(l_nodes), Self::Some(r_nodes)) => l_nodes == r_nodes,
            (
                Self::None {
                    code_index: l_code_index,
                },
                Self::None {
                    code_index: r_code_index,
                },
            ) => l_code_index == r_code_index,
            _ => false,
        }
    }
}

impl<T> Eq for RepeatingNodeRef<T> {}

impl<T> CodeSpan for RepeatingNodeRef<T>
where
    NodeRef<T>: CodeSpan,
{
    fn recurse_span_start<'a>(
        &self,
        syntax_tree: &'a SyntaxTree,
    ) -> Result<usize, &'a dyn CodeSpan> {
        match self {
            RepeatingNodeRef::Some(nodes) => nodes
                .first()
                .expect("repeating node should have at least one node")
                .recurse_span_start(syntax_tree),
            RepeatingNodeRef::None { code_index } => Ok(*code_index),
        }
    }

    fn recurse_span_end<'a>(
        &self,
        syntax_tree: &'a SyntaxTree,
    ) -> Result<CodeSpanEnd, &'a dyn CodeSpan> {
        match self {
            RepeatingNodeRef::Some(nodes) => nodes
                .last()
                .expect("repeating node should have at least one node")
                .recurse_span_end(syntax_tree),
            RepeatingNodeRef::None { code_index } => Ok(CodeSpanEnd {
                index: *code_index,
                index_with_trailing_whitespace: *code_index,
            }),
        }
    }
}

impl<T> NodeBuilderReadable for RepeatingNodeRef<T> {
    fn read(context: &mut NodeBuilderReader) -> Self {
        let mut nodes = Vec::new();
        loop {
            match context.read_content() {
                NodeBuilderContent::Node { node } => nodes.push(NodeRef::new(node.0)),
                NodeBuilderContent::EndRepetition { code_index } => {
                    break if nodes.is_empty() {
                        Self::None { code_index }
                    } else {
                        Self::Some(nodes)
                    }
                }
                NodeBuilderContent::Token { .. } | NodeBuilderContent::OptionalMismatch { .. } => {
                    panic!("repeating node expected")
                }
            }
        }
    }
}

impl<T: Visualize> Visualize for RepeatingNodeRef<T>
where
    NodeRef<T>: Visualize,
{
    fn visualize(&self, syntax_tree: &SyntaxTree, code: &str, indent: usize, with_indent: bool) {
        if with_indent {
            print_indent(indent);
        }
        if let RepeatingNodeRef::Some(nodes) = self {
            println!("[{}]", nodes.len());
            for node in nodes {
                node.visualize(syntax_tree, code, indent + 1, true);
            }
        } else {
            println!("[0]");
        }
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
pub struct Group<T> {
    opening: Token,
    content: T,
    closing: Token,
}

impl<T> CodeSpan for Group<T> {
    fn recurse_span_start<'a>(
        &self,
        syntax_tree: &'a SyntaxTree,
    ) -> Result<usize, &'a dyn CodeSpan> {
        self.opening.recurse_span_start(syntax_tree)
    }

    fn recurse_span_end<'a>(
        &self,
        syntax_tree: &'a SyntaxTree,
    ) -> Result<CodeSpanEnd, &'a dyn CodeSpan> {
        self.closing.recurse_span_end(syntax_tree)
    }
}

impl<T: NodeBuilderReadable> NodeBuilderReadable for Group<T> {
    fn read(context: &mut NodeBuilderReader) -> Self {
        Self {
            opening: context.read(),
            content: context.read(),
            closing: context.read(),
        }
    }
}

impl<T: Visualize> Visualize for Group<T> {
    fn visualize(&self, syntax_tree: &SyntaxTree, code: &str, indent: usize, with_indent: bool) {
        if with_indent {
            print_indent(indent);
        }
        print!(
            "{}...{} ",
            &code[self.opening.span(syntax_tree)],
            &code[self.closing.span(syntax_tree)],
        );
        self.content.visualize(syntax_tree, code, indent, false);
    }
}

fn print_indent(indent: usize) {
    print!("{: <1$}", "", indent);
}

#[rustfmt::skip]
macro_rules! token {
    ( + ) => { GrammarToken::Punctuation(PunctuationToken::Plus) };
    ( - ) => { GrammarToken::Punctuation(PunctuationToken::Minus) };
    ( * ) => { GrammarToken::Punctuation(PunctuationToken::Star) };
    ( / ) => { GrammarToken::Punctuation(PunctuationToken::Slash) };
    ( % ) => { GrammarToken::Punctuation(PunctuationToken::Percent) };
    ( ^ ) => { GrammarToken::Punctuation(PunctuationToken::Caret) };
    ( ! ) => { GrammarToken::Punctuation(PunctuationToken::Not) };
    ( & ) => { GrammarToken::Punctuation(PunctuationToken::And) };
    ( | ) => { GrammarToken::Punctuation(PunctuationToken::Or) };
    ( && ) => { GrammarToken::Punctuation(PunctuationToken::AndAnd) };
    ( || ) => { GrammarToken::Punctuation(PunctuationToken::OrOr) };
    ( << ) => { GrammarToken::Punctuation(PunctuationToken::Shl) };
    ( >> ) => { GrammarToken::Punctuation(PunctuationToken::Shr) };
    ( += ) => { GrammarToken::Punctuation(PunctuationToken::PlusEq) };
    ( -= ) => { GrammarToken::Punctuation(PunctuationToken::MinusEq) };
    ( *= ) => { GrammarToken::Punctuation(PunctuationToken::StarEq) };
    ( /= ) => { GrammarToken::Punctuation(PunctuationToken::SlashEq) };
    ( %= ) => { GrammarToken::Punctuation(PunctuationToken::PercentEq) };
    ( ^= ) => { GrammarToken::Punctuation(PunctuationToken::CaretEq) };
    ( &= ) => { GrammarToken::Punctuation(PunctuationToken::AndEq) };
    ( |= ) => { GrammarToken::Punctuation(PunctuationToken::OrEq) };
    ( <<= ) => { GrammarToken::Punctuation(PunctuationToken::ShlEq) };
    ( >>= ) => { GrammarToken::Punctuation(PunctuationToken::ShrEq) };
    ( = ) => { GrammarToken::Punctuation(PunctuationToken::Eq) };
    ( == ) => { GrammarToken::Punctuation(PunctuationToken::EqEq) };
    ( != ) => { GrammarToken::Punctuation(PunctuationToken::Ne) };
    ( > ) => { GrammarToken::Punctuation(PunctuationToken::Gt) };
    ( < ) => { GrammarToken::Punctuation(PunctuationToken::Lt) };
    ( >= ) => { GrammarToken::Punctuation(PunctuationToken::Ge) };
    ( <= ) => { GrammarToken::Punctuation(PunctuationToken::Le) };
    ( @ ) => { GrammarToken::Punctuation(PunctuationToken::At) };
    ( . ) => { GrammarToken::Punctuation(PunctuationToken::Dot) };
    ( .. ) => { GrammarToken::Punctuation(PunctuationToken::DotDot) };
    ( .= ) => { GrammarToken::Punctuation(PunctuationToken::DotEq) };
    ( ... ) => { GrammarToken::Punctuation(PunctuationToken::DotDotDot) };
    ( ..= ) => { GrammarToken::Punctuation(PunctuationToken::DotDotEq) };
    ( , ) => { GrammarToken::Punctuation(PunctuationToken::Comma) };
    ( ; ) => { GrammarToken::Punctuation(PunctuationToken::Semi) };
    ( : ) => { GrammarToken::Punctuation(PunctuationToken::Colon) };
    ( :: ) => { GrammarToken::Punctuation(PunctuationToken::PathSep) };
    ( -> ) => { GrammarToken::Punctuation(PunctuationToken::RArrow) };
    ( => ) => { GrammarToken::Punctuation(PunctuationToken::FatArrow) };
    ( # ) => { GrammarToken::Punctuation(PunctuationToken::Pound) };
    ( $ ) => { GrammarToken::Punctuation(PunctuationToken::Dollar) };
    ( ? ) => { GrammarToken::Punctuation(PunctuationToken::Question) };
    ( ~ ) => { GrammarToken::Punctuation(PunctuationToken::Tilde) };

    ( as ) => { GrammarToken::Keyword(KeywordToken::As) };
    ( break ) => { GrammarToken::Keyword(KeywordToken::Break) };
    ( continue ) => { GrammarToken::Keyword(KeywordToken::Continue) };
    ( crate ) => { GrammarToken::Keyword(KeywordToken::Crate) };
    ( else ) => { GrammarToken::Keyword(KeywordToken::Else) };
    ( false ) => { GrammarToken::Keyword(KeywordToken::False) };
    ( fn ) => { GrammarToken::Keyword(KeywordToken::Fn) };
    ( for ) => { GrammarToken::Keyword(KeywordToken::For) };
    ( if ) => { GrammarToken::Keyword(KeywordToken::If) };
    ( impl ) => { GrammarToken::Keyword(KeywordToken::Impl) };
    ( in ) => { GrammarToken::Keyword(KeywordToken::In) };
    ( let ) => { GrammarToken::Keyword(KeywordToken::Let) };
    ( loop ) => { GrammarToken::Keyword(KeywordToken::Loop) };
    ( match ) => { GrammarToken::Keyword(KeywordToken::Match) };
    ( mod ) => { GrammarToken::Keyword(KeywordToken::Mod) };
    ( pub ) => { GrammarToken::Keyword(KeywordToken::Pub) };
    ( return ) => { GrammarToken::Keyword(KeywordToken::Return) };
    ( self ) => { GrammarToken::Keyword(KeywordToken::SelfValue) };
    ( Self ) => { GrammarToken::Keyword(KeywordToken::SelfType) };
    ( struct ) => { GrammarToken::Keyword(KeywordToken::Struct) };
    ( super ) => { GrammarToken::Keyword(KeywordToken::Super) };
    ( trait ) => { GrammarToken::Keyword(KeywordToken::Trait) };
    ( true ) => { GrammarToken::Keyword(KeywordToken::True) };
    ( type ) => { GrammarToken::Keyword(KeywordToken::Type) };
    ( _ ) => { GrammarToken::Keyword(KeywordToken::Underscore) };
    ( use ) => { GrammarToken::Keyword(KeywordToken::Use) };
    ( while ) => { GrammarToken::Keyword(KeywordToken::While) };

    ( $name:ident ) => { GrammarToken::$name };
}

macro_rules! rule {
    ( $grammar:ident ( $( $inner:tt )* ) ) => { rule!($grammar $( $inner )* ) };
    ( $grammar:ident [ $( $inner:tt )* ] ) => { rule!($grammar $( $inner )* ) };
    ( $grammar:ident { $( $inner:tt )* } ) => { rule!($grammar $( $inner )* ) };

    ( $grammar:ident ref $T:ident ) => { Rule::Ref($grammar.rule::<$T>()) };
    ( $grammar:ident $( $token:tt )* ) => { Rule::Token(token!( $( $token )* )) };
}

#[rustfmt::skip]
macro_rules! group_kind {
    ( () ) => { GroupKind::Paren };
    ( [] ) => { GroupKind::Brack };
    ( {} ) => { GroupKind::Curly };
    ( < ) => { GroupKind::Angle };
}

#[rustfmt::skip]
macro_rules! some_group_kind {
    ( $group:tt ) => { Some(group_kind!($group)) };
    () => { None };
}

macro_rules! group_rule {
    ( $grammar:ident ( $( $group:tt )? ) $( $Field:tt )* ) => {
        GroupRule {
            rule: rule!($grammar $( $Field )* ),
            group: some_group_kind!( $( $group )? ),
        }
    };
}

macro_rules! concatenation_rule {
    ( $grammar:ident $Kind:ident ( $( $Field:tt )* ) $( $group:tt )? ) => {
        paste! {
            [<$Kind Rule>] {
                group_rule: group_rule!($grammar ( $( $group )? ) $( $Field )* ),
                parse_mode: [<$Kind RuleMode>]::$Kind,
            }
        }
    };
    ( $grammar:ident $Kind:ident [ $( $Field:tt )* ] $( $group:tt )? ) => {
        paste! {
            [<$Kind Rule>] {
                group_rule: group_rule!($grammar ( $( $group )? ) $( $Field )* ),
                parse_mode: [<$Kind RuleMode>]::Optional,
            }
        }
    };
    ( $grammar:ident $Kind:ident { $( $Field:tt )* } $( $group:tt )? ) => {
        paste! {
            [<$Kind Rule>] {
                group_rule: group_rule!($grammar ( $( $group )? ) $( $Field )* ),
                parse_mode: [<$Kind RuleMode>]::Repetition,
            }
        }
    };

    ( $grammar:ident $Kind:ident (ref $T:ident) $( $group:tt )? ) => {
        paste! {
            [<$Kind Rule>] {
                group_rule: group_rule!($grammar ( $( $group )? ) ref $T),
                parse_mode: [<$Kind RuleMode>]::$Kind,
            }
        }
    };
    ( $grammar:ident $Kind:ident [ref $T:ident] $( $group:tt )? ) => {
        paste! {
            [<$Kind Rule>] {
                group_rule: group_rule!($grammar ( $( $group )? ) ref $T),
                parse_mode: [<$Kind RuleMode>]::Optional,
            }
        }
    };
    ( $grammar:ident $Kind:ident { ref $T:ident } $( $group:tt )? ) => {
        paste! {
            [<$Kind Rule>] {
                group_rule: group_rule!($grammar ( $( $group )? ) ref $T),
                parse_mode: [<$Kind RuleMode>]::Repetition,
            }
        }
    };

    ( $grammar:ident $Kind:ident $Field:tt $( $group:tt )? ) => {
        paste! {
            [<$Kind Rule>] {
                group_rule: group_rule!($grammar ( $( $group )? ) $Field),
                parse_mode: [<$Kind RuleMode>]::$Kind,
            }
        }
    };
}

#[rustfmt::skip]
macro_rules! essential_rule_mode {
    ( ( $( $_:tt )* ) ) => { EssentialRuleMode::Essential };
    ( [ $( $_:tt )* ] ) => { EssentialRuleMode::Optional };
    ( { $( $_:tt )* } ) => { EssentialRuleMode::Repetition };
    ( $( $_:tt )* ) => { EssentialRuleMode::Essential };
}

macro_rules! last_essential_rule {
    ( $grammar:ident $Field:tt ) => {
        LastEssentialRule::Rule(rule!($grammar $Field))
    };
    ( $grammar:ident $Field:tt $group:tt ) => {
        LastEssentialRule::Grouped {
            rule: rule!($grammar $Field),
            group_kind: group_kind!($group),
            parse_mode: essential_rule_mode!($Field),
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

macro_rules! field_type {
    ( (ref $T:ident) ) => { NodeRef<$T> };
    ( [ref $T:ident] ) => { OptionalNodeRef<$T> };
    ( { ref $T:ident } ) => { RepeatingNodeRef<$T> };
    ( ( $( $rule:tt )* ) ) => { Token };
    ( [ $( $rule:tt )* ] ) => { OptionalToken };
    ( { $( $rule:tt )* } ) => { RepeatingToken };
    ( $( $rule:tt )* ) => { Token };
}

macro_rules! field {
    ( () $T:tt ) => { Group<field_type!($T)> };
    ( [] $T:tt ) => { Group<field_type!($T)> };
    ( {} $T:tt ) => { Group<field_type!($T)> };
    ( < $T:tt ) => { Group<field_type!($T)> };
    ( $T:tt ) => { field_type!($T) };
}

macro_rules! impl_copy_if_no_repetition {
    ( $T:ident { $( $first:tt )* } $( $rest:tt )* ) => {};
    ( $T:ident $first:tt $( $rest:tt )* ) => {
        impl_copy_if_no_repetition!( $T $( $rest )* );
    };
    ( $T:ident ) => {
        impl Copy for $T {}
    };
}

macro_rules! first {
    ( $self:ident $first:ident $( $rest:ident )* ) => {
        $self.$first
    };
}

macro_rules! last {
    ( $self:ident $first:ident $( $rest:ident )+ ) => {
        last!( $self $( $rest )+ )
    };
    ( $self:ident $last:ident ) => {
        $self.$last
    };
}

macro_rules! global_field {
    ( { ref $T:ident } ) => { RepeatingNodeRef<$T> };
    ( { $( $rule:tt )* } ) => { RepeatingToken };
}

macro_rules! global_rule {
    ( $grammar:ident { ref $T:ident } ) => { Rule::Ref($grammar.rule::<$T>()) };
    ( $grammar:ident { $( $token:tt )* } ) => { Rule::Token(token!( $( $token )* )) };
}

macro_rules! global {
    ( $( #[ $( $meta:meta )* ] )* $T:ident {
        $( #[ $( $field_meta:meta )* ] )* $field:ident : $Field:tt
    } ) => {
        $( #[ $( $meta )* ] )*
        #[derive(Clone, Debug)]
        pub struct $T {
            $( #[ $( $field_meta )* ] )*
            pub $field: global_field!($Field),
        }

        // No impl_copy_if_no_repetition, since this always has a single repetition

        impl $T {
            pub fn grammar() -> &'static Grammar {
                lazy_static! {
                    static ref GRAMMAR: Grammar = Grammar::new::<$T>();
                }
                &GRAMMAR
            }
        }

        impl CodeSpan for $T {
            fn recurse_span_start<'a>(
                &self,
                syntax_tree: &'a SyntaxTree,
            ) -> Result<usize, &'a dyn CodeSpan> {
               self.$field.recurse_span_start(syntax_tree)
            }

            fn recurse_span_end<'a>(
                &self,
                syntax_tree: &'a SyntaxTree,
            ) -> Result<CodeSpanEnd, &'a dyn CodeSpan> {
                self.$field.recurse_span_end(syntax_tree)
            }
        }

        impl Visualize for $T {
            fn visualize(
                &self,
                syntax_tree: &SyntaxTree,
                code: &str,
                mut indent: usize,
                with_indent: bool,
            ) {
                if with_indent {
                    print_indent(indent);
                }
                println!("{}", <Self as Buildable>::name());
                indent += 1;
                print_indent(indent);
                print!("{}: ", stringify!($field));
                self.$field.visualize(syntax_tree, code, indent, false);
            }
        }

        impl_node_ref_helpers!($T);

        impl Buildable for $T {
            fn name() -> &'static str {
                stringify!($T)
            }

            fn rule(#[allow(unused_variables)] grammar: &mut GrammarBuilder) -> RecursiveRule {
                RecursiveRule::Global(GlobalRule {
                    rule: global_rule!(grammar $Field),
                })
            }

            paste! {
                fn build(
                    syntax_tree: &mut SyntaxTree,
                    reader: &mut NodeBuilderReader,
                ) -> UntypedNodeRef {
                    let node_ref = UntypedNodeRef(syntax_tree.nodes.[<$T:snake _nodes>].len());
                    syntax_tree.nodes.[<$T:snake _nodes>].push(Self {
                        $field: reader.read(),
                    });
                    node_ref
                }
            }
        }
    };
}

macro_rules! concatenation {
    ( $( #[ $( $meta:meta )* ] )* $T:ident {
        // the > separator must come before any attributes, as it introduces macro ambiguity otherwise
        $( $( #[ $( $essential_fields_meta:meta )* ] )* $essential_fields:ident $( in $essential_field_groups:tt )? : $EssentialFields:tt )*
        > $( #[ $( $last_essential_field_meta:meta )* ] )* $last_essential_field:ident $( in $last_essential_field_group:tt )? : $LastEssentialField:tt
        $( $( #[ $( $required_fields_meta:meta )* ] )* $required_fields:ident $( in $required_field_groups:tt )? : $RequiredFields:tt )*
    } ) => {
        $( #[ $( $meta )* ] )*
        #[derive(Clone, Debug)]
        pub struct $T {
            $(
                $( #[ $( $essential_fields_meta )* ] )*
                pub $essential_fields: field!( $( $essential_field_groups )? $EssentialFields),
            )*
            $( #[ $( $last_essential_field_meta )* ] )*
            pub $last_essential_field: field!( $( $last_essential_field_group )? $LastEssentialField),
            $(
                $( #[ $( $required_fields_meta )* ] )*
                pub $required_fields: field!( $( $required_field_groups )? $RequiredFields),
            )*
        }

        impl_copy_if_no_repetition! {
            $T
            $( $EssentialFields )*
            $LastEssentialField
            $( $RequiredFields )*
        }

        impl $T {
            pub fn grammar() -> &'static Grammar {
                lazy_static! {
                    static ref GRAMMAR: Grammar = Grammar::new::<$T>();
                }
                &GRAMMAR
            }
        }

        impl CodeSpan for $T {
            fn recurse_span_start<'a>(
                &self,
                syntax_tree: &'a SyntaxTree,
            ) -> Result<usize, &'a dyn CodeSpan> {
                first!( self $( $essential_fields )* $last_essential_field).recurse_span_start(syntax_tree)
            }

            fn recurse_span_end<'a>(
                &self,
                syntax_tree: &'a SyntaxTree,
            ) -> Result<CodeSpanEnd, &'a dyn CodeSpan> {
                last!( self $( $essential_fields )* $last_essential_field).recurse_span_end(syntax_tree)
            }
        }

        impl Visualize for $T {
            fn visualize(
                &self,
                syntax_tree: &SyntaxTree,
                code: &str,
                mut indent: usize,
                with_indent: bool,
            ) {
                if with_indent {
                    print_indent(indent);
                }
                println!("{}", <Self as Buildable>::name());
                indent += 1;
                $(
                    print_indent(indent);
                    print!("{}: ", stringify!($essential_fields));
                    self.$essential_fields.visualize(syntax_tree, code, indent, false);
                )*
                print_indent(indent);
                print!("{}: ", stringify!($last_essential_field));
                self.$last_essential_field.visualize(syntax_tree, code, indent, false);
                $(
                    print_indent(indent);
                    print!("{}: ", stringify!($required_fields));
                    self.$required_fields.visualize(syntax_tree, code, indent, false);
                )*
            }
        }

        impl_node_ref_helpers!($T);

        impl Buildable for $T {
            fn name() -> &'static str {
                stringify!($T)
            }

            fn rule(#[allow(unused_variables)] grammar: &mut GrammarBuilder) -> RecursiveRule {
                RecursiveRule::Concatenation {
                    essential: vec![ $( concatenation_rule!(grammar Essential $EssentialFields $( $essential_field_groups )? ), )* ].into_boxed_slice(),
                    last_essential: last_essential_rule!(grammar $LastEssentialField $( $last_essential_field_group )? ),
                    required: vec![ $( concatenation_rule!(grammar Required $RequiredFields $( $required_field_groups )? ), )* ].into_boxed_slice(),
                }
            }

            paste! {
                fn build(
                    syntax_tree: &mut SyntaxTree,
                    reader: &mut NodeBuilderReader,
                ) -> UntypedNodeRef {
                    let node_ref = UntypedNodeRef(syntax_tree.nodes.[<$T:snake _nodes>].len());
                    syntax_tree.nodes.[<$T:snake _nodes>].push(Self {
                        $( $essential_fields: reader.read(), )*
                        $last_essential_field: reader.read(),
                        $( $required_fields: reader.read(), )*
                    });
                    node_ref
                }
            }
        }
    };
}

macro_rules! alternation {
    ( $( #[ $( $meta:meta )* ] )* $T:ident {
        $( $( #[ $( $alternative_meta:meta )* ] )* $alternative:ident: $Alternative:tt )*
    } ) => {
        $( #[ $( $meta )* ] )*
        #[derive(Clone, Debug)]
        pub enum $T {
            $(
                $( #[ $( $alternative_meta )* ] )*
                $alternative(field!($Alternative)),
            )*
        }

        impl_copy_if_no_repetition!($T $( $Alternative )* );

        impl $T {
            pub fn grammar() -> &'static Grammar {
                lazy_static! {
                    static ref GRAMMAR: Grammar = Grammar::new::<$T>();
                }
                &GRAMMAR
            }
        }

        impl CodeSpan for $T {
            fn recurse_span_start<'a>(
                &self,
                syntax_tree: &'a SyntaxTree,
            ) -> Result<usize, &'a dyn CodeSpan> {
                match self {
                    $( Self::$alternative(node_or_token) => {
                        node_or_token.recurse_span_start(syntax_tree)
                    } )*
                }
            }

            fn recurse_span_end<'a>(
                &self,
                syntax_tree: &'a SyntaxTree,
            ) -> Result<CodeSpanEnd, &'a dyn CodeSpan> {
                match self {
                    $( Self::$alternative(node_or_token) => {
                        node_or_token.recurse_span_end(syntax_tree)
                    } )*
                }
            }
        }

        impl Visualize for $T {
            fn visualize(
                &self,
                syntax_tree: &SyntaxTree,
                code: &str,
                indent: usize,
                with_indent: bool,
            ) {
                if with_indent {
                    print_indent(indent);
                }
                print!("{}::", <Self as Buildable>::name());
                match &self {
                    $( Self::$alternative(node_or_token) => {
                        print!("{} > ", stringify!($alternative));
                        node_or_token.visualize(syntax_tree, code, indent, false);
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
                RecursiveRule::Alternation {
                    variants: vec![ $( AlternationRule {
                        rule: rule!(grammar $Alternative),
                    }, )* ].into_boxed_slice()
                }
            }

            paste! {
                fn build(
                    syntax_tree: &mut SyntaxTree,
                    reader: &mut NodeBuilderReader,
                ) -> UntypedNodeRef {
                    let node_ref = UntypedNodeRef(syntax_tree.nodes.[<$T:snake _nodes>].len());

                    let alternations = [ $(
                        (|syntax_tree: &mut SyntaxTree, reader: &mut NodeBuilderReader| {
                            syntax_tree.nodes.[<$T:snake _nodes>].push(
                                Self::$alternative(reader.read())
                            );
                        }) as fn(&mut SyntaxTree, &mut NodeBuilderReader),
                    )* ];

                    alternations[reader.read_alternation_index()](syntax_tree, reader);

                    node_ref
                }
            }
        }
    };
}

macro_rules! recursive_rule {
    ( $( #[ $( $meta:meta )* ] )* mod $T:ident $body:tt ) => {
        global!( $( #[ $( $meta )* ] )* $T $body);
    };
    ( $( #[ $( $meta:meta )* ] )* struct $T:ident $body:tt ) => {
        concatenation!( $( #[ $( $meta )* ] )* $T $body);
    };
    ( $( #[ $( $meta:meta )* ] )* enum $T:ident $body:tt ) => {
        alternation!( $( #[ $( $meta )* ] )* $T $body);
    };
}

macro_rules! syntax_tree_nodes {
    ( $( $( #[ $( $meta:meta )* ] )* $kind:ident $T:ident $body:tt )* ) => {
        paste! {
            #[derive(Debug, Default)]
            struct SyntaxTreeNodes {
                $( [<$T:snake _nodes>]: Vec<$T>, )*
            }
        }

        $( recursive_rule!( $( #[ $( $meta )* ] )* $kind $T $body); )*
    };
}

syntax_tree_nodes! {

enum AtomicExpression {
    True: true
    False: false
    Integer: Integer
    Float: Float
    String: String
    Char: Char
    Path: (ref Path)
    Struct: (ref StructExpression)
    Group: (ref GroupExpression)
    Block: (ref BlockExpression)
    Return: (ref ReturnExpression)
    Break: (ref BreakExpression)
    Continue: (ref ContinueExpression)
}

struct BinaryOperation {
  > operator: (ref BinaryOperator)
    expression: (ref PrefixExpression)
}

enum BinaryOperator {
    Add: +
    Sub: -
    Mul: *
    Div: /
    Rem: %
    BitAnd: &
    BitOr: |
    BitXor: ^
    Shl: <<
    Shr: >>
    Eq: ==
    Ne: !=
    Gt: >
    Lt: <
    Ge: >=
    Le: <=
    And: &&
    Or: ||
    Assign: =
    AddAssign: +=
    SubAssign: -=
    MulAssign: *=
    DivAssign: /=
    RemAssign: %=
    BitAndAssign: &=
    BitOrAssign: |=
    BitXorAssign: ^=
    ShlAssign: <<=
    ShrAssign: >>=
}

/// A plain block that cannot have a label attached.
///
/// See [`LabeledBlock`] if the block might have a label.
struct Block {
    > content in {}: { ref Statement }
}

/// Expressions that come with a block.
enum BlockExpression {
    Block: (ref LabeledBlock)
    For: (ref ForLoop)
    If: (ref IfBlock)
    Loop: (ref LoopBlock)
    While: (ref WhileLoop)
}

/// A label followed by a colon, used for labeling blocks.
struct BlockLabel {
  > label: Label
    colon: :
}

struct BreakExpression {
  > break_kw: break
    label: [Label]
    expression: [ref Expression]
}

struct ContinueExpression {
  > continue_kw: continue
    label: [Label]
}

struct ElseBlock {
  > else_kw: else
    block: (ref Block)
}

struct ElseIfBlock {
    else_kw: else
  > if_kw: if
    condition: (ref Expression)
    block: (ref Block)
}

struct Expression {
  > expression: (ref PrefixExpression)
    suffix: { ref ExpressionSuffix }
}

enum ExpressionPrefix {
    Unary: (ref UnaryOperator)
}

struct ExpressionStatement {
  > expression: (ref Expression)
    semi: [;]
}

enum ExpressionSuffix {
    Binary: (ref BinaryOperation)
    // FieldAccess: (ref FieldAccess)
    StructCall: (ref StructExpression)
    ValueCall: (ref GroupExpression)
    // ArrayCall: (ref ArrayExpression)
    // A theoretical `BlockCall` would cause ambiguities with `if <expression> { }`
}

struct ForLoop {
    label: [ref BlockLabel]
  > for_kw: for
    name: Ident
    in_kw: in
    iterable: (ref Expression)
    block: (ref Block)
}

struct Function {
    pub_kw: [pub]
  > fn_kw: fn
    name: Ident
    // TODO: parameter
    return_type: [ref ReturnType]
    body: (ref Block)
}

struct GroupExpression {
  > expression in (): (ref Expression)
}

/// An if block followed by a series of else-if blocks and an optional final else block.
struct IfBlock {
  > if_kw: if
    condition: (ref Expression)
    then_block: (ref Block)
    else_if_blocks: { ref ElseIfBlock }
    else_block: [ref ElseBlock]
}

enum Item {
    Use: (ref Use)
    SubModule: (ref SubModule)
    Function: (ref Function)
}

/// An optional label followed by a block.
struct LabeledBlock {
    label: [ref BlockLabel]
  > block: (ref Block)
}

struct Let {
  > let_kw: let
    name: Ident
    eq: =
    value: (ref Expression)
    semi: ;
}

struct LoopBlock {
    label: [ref BlockLabel]
  > loop_kw: loop
    block: (ref Block)
}

mod Module {
    items: { ref Item }
}

struct Path {
    // TODO: possibly handle keyword prefixes like crate:: super:: self:: etc...
    path_sep: [::]
  > name: Ident
    segments: { ref PathSegment }
}

struct PathSegment {
  > path_sep: ::
    name: Ident
}

struct PrefixExpression {
    prefix: { ref ExpressionPrefix }
  > expression: (ref AtomicExpression)
}

struct ReturnExpression {
  > return_kw: return
    expression: [ref Expression]
}

struct ReturnType {
  > r_arrow: ->
    ty: (ref Type)
}

enum Statement {
    /// Just a semicolon.
    Empty: ;
    Let: (ref Let)
    Item: (ref StatementItem)
    Expression: (ref ExpressionStatement)
}

/// [`Item`]s that can also be put inside of [`Block`]s.
enum StatementItem {
    Use: (ref Use)
    Function: (ref Function)
}

struct StructExpression {
  > fields in (): { ref StructExpressionField }
}

struct StructExpressionField {
    /// The name can be ommited if it can be determinted from the value.
    name: [Ident]
  > colon: :
    value: (ref Expression)
    comma: [,]
}

struct SubModule {
  > mod_kw: mod
    name: Ident
    items in {}: { ref Item }
}

struct Type {
  > path: (ref Path)
}

enum UnaryOperator {
    Neg: -
    Not: !
}

struct Use {
  > use_kw: use
    // TODO: use path tree
    semi: ;
}

struct WhileLoop {
    label: [ref BlockLabel]
  > while_kw: while
    condition: (ref Expression)
    block: (ref Block)
}

}
