use std::any::{Any, TypeId};

use crate::compiler::lexer::{BraceKind, TokenKind};

use super::syntax_tree_node_builder::{Buildable, NodeBuilder};

#[derive(Debug)]
pub struct Grammar {
    initial_rule: RuleRef,
    rules: Vec<NamedRule>,
}

#[derive(Debug, Default)]
pub struct GrammarBuilder {
    rules: Vec<(TypeId, Option<NamedRule>)>,
}

#[derive(Clone, Debug)]
pub struct NamedRule {
    pub name: &'static str,
    pub rule: RecursiveRule,
    pub builder: NodeBuilder,
}

#[derive(Clone, Debug)]
pub enum RecursiveRule {
    Concatenation {
        essential: Vec<OptionalRule>,
        last_essential: Rule,
        required: Vec<OptionalRule>,
    },
    Alternation(Vec<Rule>),
    Repetition(Rule),
    GlobalRepetition(Rule),
    BracedRepetition(BraceKind, Rule),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OptionalRule {
    Optional(Rule),
    Required(Rule),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Rule {
    Token(TokenKind),
    Ref(RuleRef),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RuleRef(usize);

impl Grammar {
    pub fn new<T: Buildable + Any>() -> Self {
        let mut result = GrammarBuilder::default();
        Grammar {
            initial_rule: result.rule::<T>(),
            rules: result
                .rules
                .into_iter()
                .map(|(_, rule)| rule.expect("all rules should be created"))
                .collect(),
        }
    }

    pub fn initial_rule(&self) -> RuleRef {
        self.initial_rule
    }

    pub fn rule(&self, rule_ref: RuleRef) -> &NamedRule {
        &self.rules[rule_ref.0]
    }
}

impl GrammarBuilder {
    pub fn rule<T: Buildable + Any>(&mut self) -> RuleRef {
        if let Some(i) = self
            .rules
            .iter()
            .enumerate()
            .find_map(|(i, (type_id, _))| (*type_id == TypeId::of::<T>()).then_some(i))
        {
            RuleRef(i)
        } else {
            let i = self.rules.len();
            self.rules.push((TypeId::of::<T>(), None));
            let rule = T::rule(self);
            let (_, named_rule) = &mut self.rules[i];
            *named_rule = Some(NamedRule {
                name: T::name(),
                rule,
                builder: NodeBuilder(T::build),
            });
            RuleRef(i)
        }
    }
}
