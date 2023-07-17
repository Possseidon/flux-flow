use std::any::{Any, TypeId};

use derive_more::From;

use crate::compiler::lexer::{GrammarToken, GroupKind};

use super::node_builder::{Buildable, NodeBuilder};

/// A grammar that defines parsing rules for a language, somewhat similar to what an EBNF is.
///
/// Contains a set of rules, as well as a reference to an initial rule from which parsing starts.
#[derive(Debug)]
pub struct Grammar {
    initial_rule: RuleRef,
    rules: Box<[NamedRule]>,
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
#[derive(Clone, Debug, From)]
pub enum RecursiveRule {
    /// Parses a single rule repeatedly until the end of the input is reached.
    Global(GlobalRule),
    /// Parses all given rules in order.
    ///
    /// 1. `essential`: Essential rules do not cause an immediate hard error.
    /// 2. `last_essential`: One essential rule must exist; all remaining rules are required.
    /// 3. `required`: Required rules cause an immediate hard error.
    Concatenation {
        essential: Box<[EssentialRule]>,
        // TODO: Ensure that this can never be optional or repetition through the type system.
        //       But GlobalRepetition is allowed, but only if it only makes sense if it isn't
        //       followed by anything.
        last_essential: EssentialRule,
        required: Box<[RequiredRule]>,
    },
    /// Tries to parse the given rules in order until one matches.
    Alternation { variants: Box<[AlternationRule]> },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GlobalRule {
    pub rule: Rule,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct EssentialRule {
    pub group_rule: GroupRule,
    pub parse_mode: EssentialRuleMode,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EssentialRuleMode {
    Essential,
    Optional,
    Repetition,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RequiredRule {
    pub group_rule: GroupRule,
    pub parse_mode: RequiredRuleMode,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RequiredRuleMode {
    Required,
    Optional,
    Repetition,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AlternationRule {
    pub rule: Rule,
}

/// A [Rule] that can optionally be wrapped in group tokens.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GroupRule {
    pub rule: Rule,
    pub group: Option<GroupKind>,
}

/// A parsing rule that either parses a single token or references a [NamedRule] in a [Grammar].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Rule {
    /// A single token.
    ///
    /// Group tokens are not allowed. [GroupRule] must be used instead. This ensures group tokens
    /// always come in pairs which allows for better error messages when parsing invalid code.
    Token(GrammarToken),
    /// A reference to another [NamedRule].
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
    /// Returns a [RuleRef] for the given [ConcatenationBuildable], creating it if it doesn't exist yet.
    ///
    /// This also recursively creates all referenced rules.
    /// Circular references are perfectly fine. The [GrammarBuilder]'s rules are simply [None].
    pub fn rule<T: Buildable + Any>(&mut self) -> RuleRef {
        self.rules
            .iter()
            .enumerate()
            .find_map(|(index, (type_id, _))| {
                (*type_id == TypeId::of::<T>()).then_some(RuleRef(index))
            })
            .unwrap_or_else(|| {
                let index = self.rules.len();
                self.rules.push((TypeId::of::<T>(), None));
                let rule = T::rule(self);
                let (_, named_rule) = &mut self.rules[index];
                *named_rule = Some(NamedRule {
                    name: T::name(),
                    rule,
                    builder: NodeBuilder(T::build),
                });
                RuleRef(index)
            })
    }
}
