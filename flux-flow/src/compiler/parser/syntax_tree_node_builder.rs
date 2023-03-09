use super::{
    grammar::{GrammarBuilder, RecursiveRule},
    syntax_tree::{NodeRef, SyntaxTree, Token},
};

#[derive(Debug)]
pub struct NodeBuilderError;

pub type NodeBuilderFunction =
    fn(&mut SyntaxTree, &mut NodeBuilderReader) -> Result<Option<NodeRef>, NodeBuilderError>;

pub trait Buildable {
    fn name() -> &'static str;

    fn rule(grammar: &mut GrammarBuilder) -> RecursiveRule;

    fn build(
        syntax_tree: &mut SyntaxTree,
        reader: &mut NodeBuilderReader,
    ) -> Result<Option<NodeRef>, NodeBuilderError>;
}

#[derive(Clone, Copy)]
pub struct NodeBuilder(pub NodeBuilderFunction);

impl std::fmt::Debug for NodeBuilder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SyntaxTreeNodeBuilder")
    }
}

#[derive(Clone, Copy, Debug)]
pub enum NodeBuilderElement {
    NodeRef(NodeRef),
    Token(Token),
    Empty,
    Error,
}

impl NodeBuilderElement {
    fn is_root(self) -> bool {
        match self {
            Self::NodeRef(node_ref) => node_ref.is_root(),
            _ => false,
        }
    }
}

#[derive(Debug, Default)]
pub struct NodeBuilderInput {
    pub stack: Vec<NodeBuilderElement>,
}

impl NodeBuilderInput {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn len(&self) -> usize {
        self.stack.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn pop_stack(&mut self, index: usize) {
        self.stack.drain(index..);
    }

    pub fn push(&mut self, element: NodeBuilderElement) {
        self.stack.push(element);
        // match element {
        //     NodeBuilderElement::NodeRef(node_ref) => self
        //         .stack
        //         .push(CompactNodeBuilderElement::NodeRef(node_ref)),
        //     NodeBuilderElement::Token(token) => {
        //         self.stack.push(CompactNodeBuilderElement::Token(token))
        //     }
        //     NodeBuilderElement::Empty => {
        //         if let Some(CompactNodeBuilderElement::Empty(last)) = self.stack.last_mut() {
        //             *last += 1;
        //         } else {
        //             self.stack.push(CompactNodeBuilderElement::Empty(1));
        //         }
        //     }
        //     NodeBuilderElement::Error => {
        //         if let Some(CompactNodeBuilderElement::Error(last)) = self.stack.last_mut() {
        //             *last += 1;
        //         } else {
        //             self.stack.push(CompactNodeBuilderElement::Error(1));
        //         }
        //     }
        // }
    }

    pub fn final_check(mut self) {
        let element = self
            .stack
            .pop()
            .expect("node builder input should not be empty");
        assert!(element.is_root());
        assert!(self.stack.is_empty());
    }
}

#[derive(Debug)]
pub struct NodeBuilderReader<'a> {
    pub node_builder_input: &'a mut NodeBuilderInput,
    pub node_builder_index: usize,
}

impl<'a> NodeBuilderReader<'a> {
    pub fn new(node_builder_input: &'a mut NodeBuilderInput, node_builder_index: usize) -> Self {
        Self {
            node_builder_input,
            node_builder_index,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.node_builder_index == self.node_builder_input.stack.len()
    }

    pub fn read<T: NodeBuilderReadable>(&mut self) -> Result<T, NodeBuilderError> {
        T::read(self)
    }

    fn read_node(&mut self) -> NodeBuilderElement {
        let old_index = self.node_builder_index;
        self.node_builder_index += 1;
        self.node_builder_input.stack[old_index]
        // match &mut self.node_builder_input.stack[self.node_builder_index] {
        //     CompactNodeBuilderElement::NodeRef(node_ref) => {
        //         self.node_builder_index += 1;
        //         NodeBuilderElement::NodeRef(*node_ref)
        //     }
        //     CompactNodeBuilderElement::Token(token) => {
        //         self.node_builder_index += 1;
        //         NodeBuilderElement::Token(*token)
        //     }
        //     CompactNodeBuilderElement::Empty(count) => {
        //         *count -= 1;
        //         if *count == 0 {
        //             self.node_builder_index += 1;
        //         }
        //         NodeBuilderElement::Empty
        //     }
        //     CompactNodeBuilderElement::Error(count) => {
        //         *count -= 1;
        //         if *count == 0 {
        //             self.node_builder_index += 1;
        //         }
        //         NodeBuilderElement::Error
        //     }
        // }
    }
}

pub trait NodeBuilderReadable: Sized {
    fn read(context: &mut NodeBuilderReader) -> Result<Self, NodeBuilderError>;
}

impl NodeBuilderReadable for NodeRef {
    fn read(context: &mut NodeBuilderReader) -> Result<Self, NodeBuilderError> {
        Option::<Self>::read(context)?.ok_or(NodeBuilderError)
    }
}

impl NodeBuilderReadable for Option<NodeRef> {
    fn read(context: &mut NodeBuilderReader) -> Result<Self, NodeBuilderError> {
        match context.read_node() {
            NodeBuilderElement::NodeRef(node_ref) => Ok(Some(node_ref)),
            NodeBuilderElement::Token(_) => panic!("token should be a node ref"),
            NodeBuilderElement::Empty => Ok(None),
            NodeBuilderElement::Error => Err(NodeBuilderError),
        }
    }
}

impl NodeBuilderReadable for Token {
    fn read(context: &mut NodeBuilderReader) -> Result<Self, NodeBuilderError> {
        Option::<Self>::read(context)?.ok_or(NodeBuilderError)
    }
}

impl NodeBuilderReadable for Option<Token> {
    fn read(context: &mut NodeBuilderReader) -> Result<Self, NodeBuilderError> {
        match context.read_node() {
            NodeBuilderElement::NodeRef(_) => panic!("node ref should be a token"),
            NodeBuilderElement::Token(token) => Ok(Some(token)),
            NodeBuilderElement::Empty => Ok(None),
            NodeBuilderElement::Error => Err(NodeBuilderError),
        }
    }
}

impl<T> NodeBuilderReadable for Vec<T>
where
    Option<T>: NodeBuilderReadable,
{
    fn read(context: &mut NodeBuilderReader) -> Result<Self, NodeBuilderError> {
        std::iter::from_fn(|| {
            Option::<T>::read(context).map_or_else(|error| Some(Err(error)), |value| value.map(Ok))
        })
        .collect()
    }
}
