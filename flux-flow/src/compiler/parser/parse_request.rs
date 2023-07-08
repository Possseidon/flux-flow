use crate::compiler::lexer::TokenKind;

use super::{
    grammar::{Grammar, OptionalRule, Rule, RuleRef},
    syntax_tree_node_builder::NodeBuilderStack,
};

#[derive(Debug)]
pub struct ParseRequestStack(pub Vec<ParseRequest>);

#[derive(Clone, Copy, Debug)]
pub enum BuildRequestPostAction {
    TreatMissingAsError,
    Repeat(Repeat),
    Revert(Revert),
}

#[derive(Clone, Copy, Debug)]
pub struct Repeat {
    pub token_stream_index: usize,
    pub repeat_until: RepeatUntil,
}

#[derive(Clone, Copy, Debug)]
pub struct Revert {
    pub token_stream_index: usize,
    pub alternations_on_match: bool,
    pub build_request_on_mismatch: bool,
}

#[derive(Clone, Copy, Debug)]
pub struct BuildRequest {
    pub rule_ref: RuleRef,
    pub node_builder_stack: NodeBuilderStack,
    pub post_action: BuildRequestPostAction,
}

#[derive(Clone, Copy, Debug)]
pub struct RuleRequest {
    pub rule: Rule,
    pub mode: ParseMode,
}

#[derive(Clone, Copy, Debug)]
pub enum ParseRequest {
    Rule(RuleRequest),
    Build(BuildRequest),
    // TODO: Custom(fn()),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RepeatUntil {
    /// Stops repetition immediately when a mismatch occurs.
    Mismatch,
    /// Only stops repetition when this exact token is found, skipping other tokens.
    Token(TokenKind),
    /// Only stops repetition once there are no more tokens.
    EndOfTokenStream,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParseMode {
    /// Causes an immediate error on mismatch, but continues the current build request.
    Required,
    /// Cancels the current build request on mismatch.
    Essential,
    /// Does not cause an error and continues the current build request on mismatch.
    Optional,
    /// Similar to [`ParseMode::Optional`], but pops remaining [`ParseMode::Alternation`]s on match.
    Alternation,
    /// Repeatedly pushes back the same rule and only stops once [`RepeatUntil`] matches.
    Repetition(RepeatUntil),
}

impl ParseRequestStack {
    pub fn new(grammar: &Grammar) -> Self {
        Self(vec![ParseRequest::Rule(RuleRequest {
            rule: Rule::Ref(grammar.initial_rule()),
            mode: ParseMode::Required,
        })])
    }

    pub fn pop(&mut self) -> Option<ParseRequest> {
        self.0.pop()
    }

    pub fn push(&mut self, request: ParseRequest) {
        self.0.push(request);
    }

    pub fn pop_build_request(&mut self) -> BuildRequest {
        loop {
            if let ParseRequest::Build(build_request) = self
                .0
                .pop()
                .expect("parse stack should contain a build request")
            {
                break build_request;
            }
        }
    }

    pub fn pop_remaining_alternations(&mut self) {
        while let Some(ParseRequest::Rule(RuleRequest {
            rule: _,
            mode: ParseMode::Alternation,
        })) = self.0.last()
        {
            self.0.pop();
        }
    }
}

impl Extend<ParseRequest> for ParseRequestStack {
    fn extend<T: IntoIterator<Item = ParseRequest>>(&mut self, iter: T) {
        self.0.extend(iter)
    }
}

impl ParseRequest {
    pub fn required(rule: OptionalRule) -> Self {
        match rule {
            OptionalRule::Optional(rule) => Self::Rule(RuleRequest {
                rule,
                mode: ParseMode::Optional,
            }),
            OptionalRule::Required(rule) => Self::Rule(RuleRequest {
                rule,
                mode: ParseMode::Required,
            }),
        }
    }

    pub fn essential(rule: OptionalRule) -> Self {
        match rule {
            OptionalRule::Optional(rule) => Self::Rule(RuleRequest {
                rule,
                mode: ParseMode::Optional,
            }),
            OptionalRule::Required(rule) => Self::Rule(RuleRequest {
                rule,
                mode: ParseMode::Essential,
            }),
        }
    }
}
