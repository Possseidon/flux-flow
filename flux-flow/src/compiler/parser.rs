use std::collections::BTreeSet;

use super::token_stream::{TokenKind, TokenStream, TokenizationError, TokenizationErrorKind};

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ParseErrorKind {
    EofExpected,
    UnexpectedEof,
    Tokenization(TokenizationErrorKind),
    Expected(BTreeSet<Descriptor>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ParseError {
    kind: ParseErrorKind,
    position: usize,
}

impl From<TokenizationError> for ParseError {
    fn from(value: TokenizationError) -> Self {
        ParseError {
            kind: ParseErrorKind::Tokenization(value.kind),
            position: value.position,
        }
    }
}

pub trait Parse: Sized {
    fn parse(ts: TokenStream) -> ParseResult<Self>;
    fn descriptor() -> Descriptor;
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Descriptor {
    Token(TokenKind),
    Named(&'static str),
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ParseResult<'a, T> {
    pub result: Option<(T, TokenStream<'a>)>,
    pub errors: Vec<ParseError>,
}

impl<'a, T> ParseResult<'a, T> {
    pub fn ok(result: T, ts: TokenStream<'a>) -> Self {
        Self {
            result: Some((result, ts)),
            errors: vec![],
        }
    }

    pub fn err(errors: Vec<ParseError>) -> Self {
        Self {
            result: None,
            errors,
        }
    }

    pub fn warn(result: T, ts: TokenStream<'a>, errors: Vec<ParseError>) -> Self {
        Self {
            result: Some((result, ts)),
            errors,
        }
    }
}

impl<T: Parse> Parse for Option<T> {
    fn parse(ts: TokenStream) -> ParseResult<Self> {
        let result = T::parse(ts);
        ParseResult {
            result: Some(
                result
                    .result
                    .map_or((None, ts), |(result, ts)| (Some(result), ts)),
            ),
            errors: result.errors,
        }
    }

    fn descriptor() -> Descriptor {
        T::descriptor()
    }
}

impl<T: Parse> Parse for Vec<T> {
    fn parse(mut ts: TokenStream) -> ParseResult<Self> {
        let mut vec = Vec::new();
        let mut errors = Vec::new();
        loop {
            let mut result = T::parse(ts);
            errors.append(&mut result.errors);
            match result.result {
                Some((value, updated_ts)) => {
                    vec.push(value);
                    ts = updated_ts;
                }
                None => break,
            }
        }
        ParseResult {
            result: Some((vec, ts)),
            errors,
        }
    }

    fn descriptor() -> Descriptor {
        T::descriptor()
    }
}

impl<T: Parse> Parse for Box<T> {
    fn parse(ts: TokenStream) -> ParseResult<Self> {
        let result = T::parse(ts);
        ParseResult {
            result: result.result.map(|(value, ts)| (Box::new(value), ts)),
            errors: result.errors,
        }
    }

    fn descriptor() -> Descriptor {
        T::descriptor()
    }
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct CodeParseResult<T> {
    pub value: Option<T>,
    pub errors: Vec<ParseError>,
}

pub fn parse<T: Parse>(code: &str) -> CodeParseResult<T> {
    let mut result = T::parse(TokenStream::new(code));
    if let Some((_, ts)) = result.result {
        if ts.next_non_whitespace_token().is_some() {
            result.errors.push(ParseError {
                kind: ParseErrorKind::EofExpected,
                position: ts.position(),
            })
        }
    }
    CodeParseResult {
        value: result.result.map(|(value, _)| value),
        errors: result.errors,
    }
}

/// Causes a parsing error if missing, however parsing continues as if it were optional.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Required<T: Parse>(Option<T>);

impl<T: Parse> Parse for Required<T> {
    fn parse(ts: TokenStream) -> ParseResult<Self> {
        let mut result = T::parse(ts);
        if result.result.is_none() {
            result.errors.push(ParseError {
                kind: ParseErrorKind::Expected([T::descriptor()].into()),
                position: ts.position(),
            });
        }
        ParseResult {
            result: Some(result.result.map_or((Required(None), ts), |(value, ts)| {
                (Required(Some(value)), ts)
            })),
            errors: result.errors,
        }
    }

    fn descriptor() -> Descriptor {
        T::descriptor()
    }
}
