use std::any::{Any, TypeId};

use crate::compiler::lexer::{BraceKind, TokenKind};

use super::syntax_tree_node_builder::{Buildable, NodeBuilder};

/// A grammar that defines parsing rules for a language, somewhat similar to what an EBNF is.
///
/// Contains a set of rules, as well as a reference to an initial rule from which parsing starts.
#[derive(Debug)]
pub struct Grammar {
    initial_rule: RuleRef,
    rules: Vec<NamedRule>,
}

/// Used to build up a [Grammar].
#[derive(Debug, Default)]
pub struct GrammarBuilder {
    rules: Vec<(TypeId, Option<NamedRule>)>,
}

/// A named recusrive parsing rule.
///
/// It also contains a builder that turns tokens and AST nodes into more AST nodes.
#[derive(Clone, Debug)]
pub struct NamedRule {
    pub name: &'static str,
    pub rule: RecursiveRule,
    pub builder: NodeBuilder,
}

/// A parsing rule that recursively references other rules.
#[derive(Clone, Debug)]
pub enum RecursiveRule {
    /// Parses all given rules in order.
    ///
    /// 1. `essential`: Essential rules do not cause an immediate hard error.
    /// 2. `last_essential`: At least one essential rule must exist.
    /// 3. `required`: Required rules cause an immediate hard error.
    Concatenation {
        essential: Vec<OptionalRule>,
        last_essential: Rule,
        required: Vec<OptionalRule>,
    },
    /// Tries to parse the given rules in order until one matches.
    Alternation(Vec<Rule>),
    /// Repeatedly parses the given rule until it no longer matches.
    Repetition(Rule),
    /// Special case of a repetition, that consumes the entire token stream.
    GlobalRepetition(Rule),
    BracedRepetition(BraceKind, Rule),
}

/// A [Rule] that might be either optional or required.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OptionalRule {
    Optional(Rule),
    Required(Rule),
}

/// A parsing rule that either parses a single token or references a [NamedRule] in a [Grammar].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Rule {
    Token(TokenKind),
    Ref(RuleRef),
}

/// References a [NamedRule] in a [Grammar].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RuleRef(usize);

impl Grammar {
    /// Creates a new `Grammar` instance using the given [Buildable] as an initial rule.
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

    /// The rule, where parsing begins.
    pub fn initial_rule(&self) -> RuleRef {
        self.initial_rule
    }

    /// Accesses a [NamedRule] using a [RuleRef].
    pub fn rule(&self, rule_ref: RuleRef) -> &NamedRule {
        &self.rules[rule_ref.0]
    }
}

impl GrammarBuilder {
    /// Returns a [RuleRef] for the given [Buildable], creating it if it doesn't exist yet.
    ///
    /// This also recursively creates all referenced rules.
    /// Circular references are perfectly fine. The [GrammarBuilder]'s rules are simply [None].
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
