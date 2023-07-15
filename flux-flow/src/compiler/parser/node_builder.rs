use crate::compiler::parser::syntax_tree::CodeSpanWithoutSyntaxTree;

use super::{
    grammar::{GrammarBuilder, RecursiveRule},
    parse_request::ParseMode,
    syntax_tree::{SyntaxTree, Token},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UntypedNodeRef(pub usize);

pub trait Buildable {
    fn name() -> &'static str;
    fn rule(grammar: &mut GrammarBuilder) -> RecursiveRule;
    fn build(syntax_tree: &mut SyntaxTree, reader: &mut NodeBuilderReader) -> UntypedNodeRef;
}

pub type NodeBuilderFunction = fn(&mut SyntaxTree, &mut NodeBuilderReader) -> UntypedNodeRef;

#[derive(Clone, Copy)]
pub struct NodeBuilder(pub NodeBuilderFunction);

impl std::fmt::Debug for NodeBuilder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NodeBuilder")
    }
}

#[derive(Clone, Copy, Debug)]
pub enum NodeBuilderContent {
    /// A valid token.
    Token { token: Token },
    /// A valid node.
    Node { node: UntypedNodeRef },
    /// An optional token that did not match.
    OptionalMismatch { code_index: usize },
    /// Marks the end of a repetition.
    EndRepetition { code_index: usize },
}

// TODO: Consider merging consecutive equal elements together again by storing a `additional_copies: usize`.
#[derive(Clone, Copy, Debug)]
enum NodeBuilderElement {
    /// Marks the start of a concatenations.
    ///
    /// Repetitions are part of concatenations and thus do not have their own variant in
    /// [`NodeBuilderElement`].
    Concatenation { code_index: usize },
    /// Marks an alternation and keeps track of mismatches as well as the total number of possible
    /// alternations.
    Alternation {
        code_index: usize,
        mismatches: usize,
        alternation_len: usize,
    },
    /// An essential token did not match.
    ///
    /// This immediately cancels the current concatenation or alternation.
    /// The corresponding root is replaced with [`NodeBuilderElement::EssentialMismatch`] and all
    /// following elements are discarded.
    Mismatch { code_index: usize },

    /// A token that matched successfully.
    Token { token: Token },
    /// A node that matched successfully.
    Node {
        node: UntypedNodeRef,
        warnings: bool,
    },
    /// An optional token that didn't match.
    OptionalMismatch { code_index: usize },
    /// Marks the end of a repetition.
    EndRepetition { code_index: usize },

    /// A required token or node that didn't match.
    ///
    /// This does not cancel the current concatenation or alternation immediately, but still
    /// prevents attempts at building a node.
    Error,
    /// A required repetition element that didn't match.
    RepetitionError,
}

impl NodeBuilderElement {
    fn is_header(self) -> bool {
        match self {
            NodeBuilderElement::Concatenation { .. }
            | NodeBuilderElement::Alternation { .. }
            | NodeBuilderElement::Mismatch { .. } => true,
            NodeBuilderElement::Token { .. }
            | NodeBuilderElement::Node { .. }
            | NodeBuilderElement::OptionalMismatch { .. }
            | NodeBuilderElement::EndRepetition { .. }
            | NodeBuilderElement::Error
            | NodeBuilderElement::RepetitionError => false,
        }
    }

    fn is_error(self) -> bool {
        matches!(self, NodeBuilderElement::Error)
    }

    fn is_repetition_error(self) -> bool {
        matches!(self, NodeBuilderElement::RepetitionError)
    }

    fn is_warning(self) -> bool {
        matches!(self, NodeBuilderElement::Node { warnings: true, .. })
    }
}

// TODO: Merging NodeAlternationMismatches and TokenAlternationMismatches might improve performance
//       they can compact with each other, which is quite common.
// TODO: Also consider if it makes sense for the other mismatch types.

#[derive(Clone, Debug, Default)]
pub struct NodeBuilderInput {
    elements: Vec<NodeBuilderElement>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProcessReaderResult {
    /// The current header was replaced with a successfully built node.
    NodeBuilt,
    /// A node was built but also did contain errors.
    ///
    /// This is used for repetitions, which can be built even if some entries failed to parse.
    NodeBuiltWithWarnings,
    /// The current header was popped and the parent header updated according to the parse mode.
    Mismatch,
    /// The current header was replaced with a call to [`NodeBuilderInput::required_mismatch()`].
    Error,
}

impl NodeBuilderInput {
    /// Creates a new [`NodeBuilderInput`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Pushes a new concatenation header.
    ///
    /// This header will only accesses elements that are added after the header has been pushed.
    pub fn push_concatenation(&mut self, code_index: usize) {
        self.elements
            .push(NodeBuilderElement::Concatenation { code_index });
    }

    /// Pushes a new alternation header.
    pub fn push_alternation(&mut self, code_index: usize, alternation_len: usize) {
        self.elements.push(NodeBuilderElement::Alternation {
            code_index,
            mismatches: 0,
            alternation_len,
        });
    }

    /// Processes the top-most header using the provided builder.
    ///
    /// Also pushes the resulting node or another appropriate element back onto the stack.
    pub fn build(
        &mut self,
        syntax_tree: &mut SyntaxTree,
        builder: NodeBuilder,
        parse_mode: ParseMode,
    ) -> ProcessReaderResult {
        let mut reader = match NodeBuilderReader::new(self, self.header_index()) {
            Ok(reader) => reader,
            Err(NodeBuilderReaderError::Mismatch { code_index }) => {
                self.pop_header();
                match parse_mode {
                    ParseMode::Essential => {
                        self.mismatch(None);
                        return ProcessReaderResult::Mismatch;
                    }
                    ParseMode::Required => {
                        self.error();
                        return ProcessReaderResult::Mismatch;
                    }
                    ParseMode::Optional => {
                        self.push_optional_mismatch(code_index);
                        return ProcessReaderResult::Mismatch;
                    }
                    ParseMode::Repetition { .. } => {
                        // cannot know yet whether to push end repetition, repetition error, etc...
                        return ProcessReaderResult::Mismatch;
                    }
                    ParseMode::Alternation => {
                        self.alternation_mismatch();
                        return ProcessReaderResult::Mismatch;
                    }
                }
            }
            Err(NodeBuilderReaderError::Error) => {
                self.pop_header();
                match parse_mode {
                    ParseMode::Repetition { .. } => self.repetition_error(),
                    _ => self.error(),
                }
                return ProcessReaderResult::Error;
            }
        };

        let warnings = reader.warnings;
        let node = builder.0(syntax_tree, &mut reader);

        self.replace_header(NodeBuilderElement::Node { node, warnings });

        if warnings {
            ProcessReaderResult::NodeBuiltWithWarnings
        } else {
            ProcessReaderResult::NodeBuilt
        }
    }

    fn header_index(&self) -> usize {
        self.elements
            .iter()
            .enumerate()
            .rev()
            .find_map(|(index, element)| element.is_header().then_some(index))
            .expect("header expected")
    }

    pub fn push_token(&mut self, token: Token) {
        self.elements.push(NodeBuilderElement::Token { token });
    }

    pub fn push_optional_mismatch(&mut self, code_index: usize) {
        self.elements
            .push(NodeBuilderElement::OptionalMismatch { code_index });
    }

    pub fn push_end_repetition(&mut self, code_index: usize) {
        self.elements
            .push(NodeBuilderElement::EndRepetition { code_index });
    }

    /// Replaces the current concatenation with a single [`NodeBuilderElement::TokenEssentialMismatch`].
    pub fn mismatch(&mut self, code_index_for_repetition: Option<usize>) -> bool {
        let header_index = self.header_index();

        let warnings = self.elements[header_index + 1..]
            .iter()
            .any(|element| element.is_warning() || element.is_repetition_error());

        if warnings {
            if let Some(code_index) = code_index_for_repetition {
                self.push_end_repetition(code_index);
            } else {
                self.error();
            }
            return false;
        }

        self.elements.drain(header_index + 1..);

        let last = self.elements.last_mut().expect("header expected");
        match last {
            NodeBuilderElement::Concatenation { code_index } => {
                *last = NodeBuilderElement::Mismatch {
                    code_index: *code_index,
                }
            }
            _ => panic!("header expected"),
        }

        true
    }

    /// Pushes a single required mismatch node.
    pub fn error(&mut self) {
        self.elements.push(NodeBuilderElement::Error);
    }

    pub fn repetition_error(&mut self) {
        self.elements.push(NodeBuilderElement::RepetitionError);
    }

    /// Increments the number of mismatched alternations.
    pub fn alternation_mismatch(&mut self) {
        match self.elements.last_mut() {
            Some(NodeBuilderElement::Alternation {
                mismatches: alternation_index,
                ..
            }) => *alternation_index += 1,
            _ => panic!("alternation expected"),
        }
    }

    /// Pops the current header including all of its content elements.
    fn pop_header(&mut self) {
        self.elements.drain(self.header_index()..);
    }

    /// Replaces the current header (including its content elements) with the provided element.
    fn replace_header(&mut self, element: NodeBuilderElement) {
        let header_index = self.header_index();
        self.elements.drain(header_index + 1..);
        self.elements[header_index] = element;
    }

    pub fn visualize(&self, code: &str) {
        print!("Nodes:");
        for node in &self.elements {
            match node {
                NodeBuilderElement::Concatenation { .. } => {
                    print!(" >")
                }
                NodeBuilderElement::Alternation {
                    mismatches,
                    alternation_len,
                    ..
                } => {
                    print!(" [{mismatches}/{alternation_len}]")
                }
                NodeBuilderElement::Mismatch { .. } => {
                    print!(" <-")
                }

                NodeBuilderElement::Token { token } => {
                    print!(" `{}`", &code[CodeSpanWithoutSyntaxTree::span(token)])
                }
                NodeBuilderElement::Node { node, warnings } => {
                    print!(" ({})", node.0);
                    if *warnings {
                        print!("!")
                    }
                }
                NodeBuilderElement::OptionalMismatch { .. } => {
                    print!(" []")
                }
                NodeBuilderElement::EndRepetition { .. } => {
                    print!(" *")
                }
                NodeBuilderElement::Error => {
                    print!(" X")
                }
                NodeBuilderElement::RepetitionError => {
                    print!(" _!")
                }
            }
        }
        println!();
    }
}

#[derive(Debug)]
pub struct NodeBuilderReader<'a> {
    node_builder_input: &'a mut NodeBuilderInput,
    node_builder_index: usize,
    warnings: bool,
}

enum NodeBuilderReaderError {
    /// The header is a mismatched essential token or node.
    Mismatch { code_index: usize },
    /// The content contains a mismatched required token or node.
    Error,
}

impl<'a> NodeBuilderReader<'a> {
    fn new(
        node_builder_input: &'a mut NodeBuilderInput,
        header_index: usize,
    ) -> Result<Self, NodeBuilderReaderError> {
        match node_builder_input.elements[header_index] {
            NodeBuilderElement::Concatenation { .. } => {
                let content_index = header_index + 1;
                if node_builder_input.elements[content_index..]
                    .iter()
                    .copied()
                    .any(NodeBuilderElement::is_error)
                {
                    Err(NodeBuilderReaderError::Error)
                } else {
                    let warnings = node_builder_input.elements[content_index..]
                        .iter()
                        .copied()
                        .any(|element| element.is_warning() || element.is_repetition_error());
                    Ok(Self {
                        node_builder_input,
                        node_builder_index: content_index,
                        warnings,
                    })
                }
            }
            NodeBuilderElement::Alternation {
                code_index,
                mismatches,
                alternation_len,
            } => {
                if mismatches == alternation_len {
                    Err(NodeBuilderReaderError::Mismatch { code_index })
                } else {
                    let content = node_builder_input.elements[header_index + 1];

                    if content.is_error() {
                        Err(NodeBuilderReaderError::Error)
                    } else {
                        Ok(Self {
                            node_builder_input,
                            node_builder_index: header_index,
                            warnings: content.is_warning() || content.is_repetition_error(),
                        })
                    }
                }
            }
            NodeBuilderElement::Mismatch { code_index } => {
                Err(NodeBuilderReaderError::Mismatch { code_index })
            }
            _ => panic!("header expected"),
        }
    }

    pub fn read<T: NodeBuilderReadable>(&mut self) -> T {
        T::read(self)
    }

    pub fn read_alternation_index(&mut self) -> usize {
        match self.read_element() {
            NodeBuilderElement::Alternation { mismatches, .. } => mismatches,
            _ => panic!("alternation index expected"),
        }
    }

    pub fn read_content(&mut self) -> NodeBuilderContent {
        loop {
            match self.read_element() {
                NodeBuilderElement::Token { token } => break NodeBuilderContent::Token { token },
                NodeBuilderElement::Node { node, .. } => break NodeBuilderContent::Node { node },
                NodeBuilderElement::OptionalMismatch { code_index } => {
                    break NodeBuilderContent::OptionalMismatch { code_index }
                }
                NodeBuilderElement::EndRepetition { code_index } => {
                    break NodeBuilderContent::EndRepetition { code_index }
                }
                NodeBuilderElement::RepetitionError => {
                    // ignore repetition errors
                }
                _ => panic!("content expected"),
            }
        }
    }

    fn read_element(&mut self) -> NodeBuilderElement {
        let node_builder_index = self.node_builder_index;
        self.node_builder_index += 1;
        self.node_builder_input.elements[node_builder_index]
    }
}

pub trait NodeBuilderReadable: Sized {
    fn read(context: &mut NodeBuilderReader) -> Self;
}
