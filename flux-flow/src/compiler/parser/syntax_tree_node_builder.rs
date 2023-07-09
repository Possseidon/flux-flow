use super::{
    grammar::{GrammarBuilder, RecursiveRule},
    syntax_tree::{EmptyToken, NodeRef, OptionalToken, SyntaxTree, Token},
};

#[derive(Debug)]
pub struct NodeBuilderError;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UntypedNodeRef(pub usize);

impl UntypedNodeRef {
    fn is_root(self) -> bool {
        self.0 == 0
    }
}

pub type NodeBuilderFunction =
    fn(&mut SyntaxTree, &mut NodeBuilderReader) -> Result<Option<UntypedNodeRef>, NodeBuilderError>;

pub trait Buildable {
    fn name() -> &'static str;

    fn rule(grammar: &mut GrammarBuilder) -> RecursiveRule;

    fn build(
        syntax_tree: &mut SyntaxTree,
        reader: &mut NodeBuilderReader,
    ) -> Result<Option<UntypedNodeRef>, NodeBuilderError>;
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
    NodeRef(UntypedNodeRef),
    Token(Token),
    EmptyToken(EmptyToken),
    Empty,
    Error,
}

#[derive(Clone, Copy, Debug)]
enum CompactNodeBuilderElement {
    NodeRef(UntypedNodeRef),
    Token(Token),
    EmptyToken(EmptyToken),
    Empty(usize),
    Error(usize),
    Separator(usize),
}

impl CompactNodeBuilderElement {
    fn is_root(self) -> bool {
        match self {
            Self::NodeRef(node_ref) => node_ref.is_root(),
            _ => false,
        }
    }
}

#[derive(Debug, Default)]
pub struct NodeBuilderInput {
    stack: Vec<CompactNodeBuilderElement>,
}

#[derive(Clone, Copy, Debug)]
pub struct NodeBuilderStack(usize);

impl NodeBuilderStack {
    fn separator(self) -> usize {
        self.0
    }

    fn after_separator(self) -> usize {
        self.0 + 1
    }
}

impl NodeBuilderInput {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_stack(&mut self) -> NodeBuilderStack {
        if let Some(CompactNodeBuilderElement::Separator(count)) = self.stack.last_mut() {
            *count += 1;
            NodeBuilderStack(self.stack.len() - 1)
        } else {
            let separator = self.stack.len();
            self.stack.push(CompactNodeBuilderElement::Separator(1));
            NodeBuilderStack(separator)
        }
    }

    pub fn pop_stack(&mut self, stack: NodeBuilderStack) {
        if let CompactNodeBuilderElement::Separator(count) = &mut self.stack[stack.separator()] {
            if *count == 1 {
                self.stack.drain(stack.separator()..);
            } else {
                *count -= 1;
                self.stack.drain(stack.after_separator()..);
            }
        } else {
            panic!("node builder stack should not contain a separator");
        }
    }

    pub fn push(&mut self, element: NodeBuilderElement) {
        match element {
            NodeBuilderElement::NodeRef(node_ref) => self
                .stack
                .push(CompactNodeBuilderElement::NodeRef(node_ref)),
            NodeBuilderElement::Token(token) => {
                self.stack.push(CompactNodeBuilderElement::Token(token))
            }
            NodeBuilderElement::EmptyToken(empty_token) => {
                self.stack
                    .push(CompactNodeBuilderElement::EmptyToken(empty_token));
            }
            NodeBuilderElement::Empty => {
                if let Some(CompactNodeBuilderElement::Empty(last)) = self.stack.last_mut() {
                    *last += 1;
                } else {
                    self.stack.push(CompactNodeBuilderElement::Empty(1));
                }
            }
            NodeBuilderElement::Error => {
                if let Some(CompactNodeBuilderElement::Error(last)) = self.stack.last_mut() {
                    *last += 1;
                } else {
                    self.stack.push(CompactNodeBuilderElement::Error(1));
                }
            }
        }
    }

    pub fn final_check(mut self) {
        let element = self
            .stack
            .pop()
            .expect("node builder input should not be empty");
        // TODO: is_root checks for index 0 put root module is last?
        //       I think it currently works because there is only one module
        assert!(element.is_root());
        assert!(self.stack.is_empty());
    }

    pub fn visualize(&self, code: &str) {
        print!("Nodes:");
        for node in &self.stack {
            match node {
                CompactNodeBuilderElement::NodeRef(node_ref) => {
                    print!(" {:?}", node_ref);
                }
                CompactNodeBuilderElement::Token(token) => {
                    print!(" `{}`", &code[token.token()]);
                }
                CompactNodeBuilderElement::EmptyToken(..) => {
                    print!(" none");
                }
                CompactNodeBuilderElement::Empty(count) => {
                    for _ in 0..*count {
                        print!(" empty");
                    }
                }
                CompactNodeBuilderElement::Error(count) => {
                    for _ in 0..*count {
                        print!(" error");
                    }
                }
                CompactNodeBuilderElement::Separator(count) => {
                    for _ in 0..*count {
                        print!(" >");
                    }
                }
            };
        }
        println!();
    }
}

#[derive(Debug)]
pub struct NodeBuilderReader<'a> {
    pub node_builder_input: &'a mut NodeBuilderInput,
    pub node_builder_index: usize,
}

impl<'a> NodeBuilderReader<'a> {
    pub fn new(
        node_builder_input: &'a mut NodeBuilderInput,
        node_builder_stack: NodeBuilderStack,
    ) -> Self {
        Self {
            node_builder_input,
            node_builder_index: node_builder_stack.after_separator(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.node_builder_index == self.node_builder_input.stack.len()
    }

    pub fn read<T: NodeBuilderReadable>(&mut self) -> Result<T, NodeBuilderError> {
        T::read(self)
    }

    fn read_node(&mut self) -> NodeBuilderElement {
        match &mut self.node_builder_input.stack[self.node_builder_index] {
            CompactNodeBuilderElement::NodeRef(node_ref) => {
                self.node_builder_index += 1;
                NodeBuilderElement::NodeRef(*node_ref)
            }
            CompactNodeBuilderElement::Token(token) => {
                self.node_builder_index += 1;
                NodeBuilderElement::Token(*token)
            }
            CompactNodeBuilderElement::EmptyToken(empty_token) => {
                self.node_builder_index += 1;
                NodeBuilderElement::EmptyToken(*empty_token)
            }
            CompactNodeBuilderElement::Empty(count) => {
                *count -= 1;
                if *count == 0 {
                    self.node_builder_index += 1;
                }
                NodeBuilderElement::Empty
            }
            CompactNodeBuilderElement::Error(count) => {
                *count -= 1;
                if *count == 0 {
                    self.node_builder_index += 1;
                }
                NodeBuilderElement::Error
            }
            CompactNodeBuilderElement::Separator(_) => {
                panic!("reader should not contain separator")
            }
        }
    }
}

pub trait NodeBuilderReadable: Sized {
    fn read(context: &mut NodeBuilderReader) -> Result<Self, NodeBuilderError>;
}

impl<T> NodeBuilderReadable for NodeRef<T> {
    fn read(context: &mut NodeBuilderReader) -> Result<Self, NodeBuilderError> {
        Option::<Self>::read(context)?.ok_or(NodeBuilderError)
    }
}

impl<T> NodeBuilderReadable for Option<NodeRef<T>> {
    fn read(context: &mut NodeBuilderReader) -> Result<Self, NodeBuilderError> {
        match context.read_node() {
            NodeBuilderElement::NodeRef(node_ref) => Ok(Some(NodeRef::new(node_ref.0))),
            NodeBuilderElement::Token(..) => panic!("token should be a node ref"),
            NodeBuilderElement::EmptyToken(..) => panic!("empty token should be a node ref"),
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
            NodeBuilderElement::NodeRef(..) => panic!("node ref should be a token"),
            NodeBuilderElement::Token(token) => Ok(Some(token)),
            NodeBuilderElement::EmptyToken(..) => panic!("empty token should be a token"),
            NodeBuilderElement::Empty => Ok(None),
            NodeBuilderElement::Error => Err(NodeBuilderError),
        }
    }
}

impl NodeBuilderReadable for OptionalToken {
    fn read(context: &mut NodeBuilderReader) -> Result<Self, NodeBuilderError> {
        match context.read_node() {
            NodeBuilderElement::NodeRef(..) => panic!("node ref should be an optional token"),
            NodeBuilderElement::Token(token) => Ok(OptionalToken::Some(token)),
            NodeBuilderElement::EmptyToken(token) => Ok(OptionalToken::None(token)),
            NodeBuilderElement::Empty => panic!("empty should be an optional token"),
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
