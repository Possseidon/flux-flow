use thiserror::Error;

use super::{
    diagnostic::{Diagnostic, ResultWithDiagnostics},
    lexer::{
        braces::*, tokens::*, BraceIndex, BraceKind, TokenIndex, TokenKind, TokenStream, TokenStreamResult,
    },
};

type ModuleParseResult<'code> = ResultWithDiagnostics<'code, Module>;

pub fn parse(mut token_stream: TokenStream) -> ResultWithDiagnostics<Module> {
    let mut diagnostics = Default::default();
    let value = Module::parse(&mut token_stream, &mut diagnostics);
    ModuleParseResult {
        code: token_stream.code(),
        value: value.ok(),
        diagnostics,
    }
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("expected {0}")]
    Expected(String),
    #[error("unexpected {}", .0.name())]
    UnexpectedToken(TokenKind),
    #[error("unexpected tokens")]
    UnexpectedTokens,
}

#[derive(Debug)]
pub enum ParseResult<T> {
    /// Parsing was successful.
    ///
    /// The TokenStream got advanced.
    Ok(T),
    /// No more tokens in the stream.
    TreeEmpty,
    /// The type could not parse, but is not a hard error.
    ///
    /// The TokenStream got reverted.
    Mismatch,
    /// The type could not parse and an error has already been pushed into diagnostics.
    ///
    /// The TokenStream advanced unless the type itself is [`Required`].
    Error,
    /// The lexer returned an error which has already been pushed into diagnostics.
    LexError,
}

impl<T> ParseResult<T> {
    fn map<U>(self, f: impl FnOnce(T) -> U) -> ParseResult<U> {
        match self {
            ParseResult::Ok(value) => ParseResult::Ok(f(value)),
            ParseResult::TreeEmpty => ParseResult::TreeEmpty,
            ParseResult::Mismatch => ParseResult::Mismatch,
            ParseResult::Error => ParseResult::Error,
            ParseResult::LexError => ParseResult::LexError,
        }
    }

    fn ok(self) -> Option<T> {
        match self {
            ParseResult::Ok(value) => Some(value),
            _ => None,
        }
    }
}

pub trait Parse: Sized {
    fn parse(
        token_stream: &mut TokenStream,
        diagnostics: &mut Vec<Diagnostic>,
    ) -> ParseResult<Self>;

    fn describe() -> String;
}

impl<T: Parse> Parse for Option<T> {
    fn parse(
        token_stream: &mut TokenStream,
        diagnostics: &mut Vec<Diagnostic>,
    ) -> ParseResult<Self> {
        match T::parse(token_stream, diagnostics) {
            ParseResult::Ok(value) => ParseResult::Ok(Some(value)),
            ParseResult::TreeEmpty | ParseResult::Mismatch => ParseResult::Ok(None),
            ParseResult::Error => ParseResult::Error,
            ParseResult::LexError => ParseResult::LexError,
        }
    }

    fn describe() -> String {
        T::describe()
    }
}

// TODO: I don't think this is necessary ever

// impl<T: Parse> Parse for Vec<T> {
//     fn parse(token_stream: &mut TokenStream, diagnostics: &mut Vec<Diagnostic>) -> ParseResult<Self> {
//         let mut values = Vec::new();
//         loop {
//             match T::parse(token_stream, diagnostics) {
//                 ParseResult::Ok(value) => values.push(value),
//                 ParseResult::TreeEmpty | ParseResult::Mismatch => break,
//                 ParseResult::Error => return ParseResult::Error,
//                 ParseResult::LexError => return ParseResult::LexError,
//             };
//         }
//         ParseResult::Ok(values)
//     }

//     fn describe() -> String {
//         format!("list of {}", T::describe())
//     }
// }

impl<T: Parse> Parse for Box<T> {
    fn parse(
        token_stream: &mut TokenStream,
        diagnostics: &mut Vec<Diagnostic>,
    ) -> ParseResult<Self> {
        T::parse(token_stream, diagnostics).map(Box::new)
    }

    fn describe() -> String {
        T::describe()
    }
}

#[derive(Debug)]
pub struct Surrounded<T: Parse, const BRACE: BraceIndex> {
    open_brace_index: usize,
    open_brace_trailing_whitespace: usize,
    value: T,
    closing_brace_index: usize,
    closing_brace_trailing_whitespace: usize,
}

impl<T: Parse, const BRACE: BraceIndex> Parse for Surrounded<T, BRACE> {
    fn parse(
        token_stream: &mut TokenStream,
        diagnostics: &mut Vec<Diagnostic>,
    ) -> ParseResult<Self> {
        match token_stream.next() {
            Some(token_tree) => match token_tree {
                Ok(token_tree) => match token_tree {
                    TokenStreamResult::Token(token) => ParseResult::Mismatch,
                    TokenStreamResult::Nested(nested_token_stream) => {
                        if nested_token_stream.surrounding_brace_kind()
                            == BraceKind::from_brace_index(BRACE)
                        {
                            let result = T::parse(&mut nested_token_stream, diagnostics);
                            let trailing_tokens_start = nested_token_stream.index();
                            token_stream.advance_token_stream(&mut nested_token_stream);
                            let trailing_tokens_range =
                                trailing_tokens_start..nested_token_stream.index();

                            if trailing_tokens_range.is_empty() {
                                result.map(Surrounded)
                            } else {
                                diagnostics.push(Diagnostic::error_range(
                                    ParseError::UnexpectedTokens,
                                    trailing_tokens_range,
                                ));
                                ParseResult::Error
                            }
                        } else {
                            ParseResult::Mismatch
                        }
                    }
                },
                Err(error) => {
                    diagnostics.push(Diagnostic::error(error, token_stream.index()));
                    ParseResult::LexError
                }
            },
            None => ParseResult::TreeEmpty,
        }
    }

    fn describe() -> String {
        format!(
            "{} enclosed {}",
            BraceKind::from_brace_index(BRACE).unwrap().name(),
            T::describe()
        )
    }
}

#[derive(Debug)]
pub struct Required<T: Parse>(pub T);

impl<T: Parse> Parse for Required<T> {
    fn parse(
        token_stream: &mut TokenStream,
        diagnostics: &mut Vec<Diagnostic>,
    ) -> ParseResult<Self> {
        match T::parse(token_stream, diagnostics) {
            ParseResult::Ok(value) => ParseResult::Ok(Required(value)),
            ParseResult::Mismatch | ParseResult::TreeEmpty => {
                diagnostics.push(Diagnostic::error(
                    ParseError::Expected(T::describe()),
                    token_stream.index(),
                ));
                ParseResult::Error
            }
            ParseResult::Error => ParseResult::Error,
            ParseResult::LexError => ParseResult::LexError,
        }
    }

    fn describe() -> String {
        T::describe()
    }
}

#[derive(Debug)]
pub struct Token<const TK: TokenIndex> {
    index: usize,
    len: usize,
    trailing_whitespace: usize,
}

impl<const TK: TokenIndex> Parse for Token<TK> {
    fn parse(
        token_stream: &mut TokenStream,
        diagnostics: &mut Vec<Diagnostic>,
    ) -> ParseResult<Self> {
        match token_stream.next() {
            Some(token_tree) => match token_tree {
                Ok(token_tree) => match token_tree {
                    TokenStreamResult::Token(token) => {
                        if token.kind == TokenKind::from_token_index(TK) {
                            let before_whitespace = token_stream.index();
                            token_stream.skip_whitespace_tokens();
                            ParseResult::Ok(Token {
                                index: token.index,
                                len: token.len,
                                trailing_whitespace: token_stream.index() - before_whitespace,
                            })
                        } else {
                            *token_stream = previous_token_stream;
                            ParseResult::Mismatch
                        }
                    }
                    TokenStreamResult::Nested(_) => todo!(),
                },
                Err(error) => {
                    diagnostics.push(Diagnostic::error(error, token_stream.index()));
                    ParseResult::LexError
                }
            },
            None => ParseResult::TreeEmpty,
        }
    }

    fn describe() -> String {
        TokenKind::from_token_index(TK).unwrap().name().into()
    }
}

macro_rules! ast_struct {
    { struct $T:ident { $( $field_name:ident: $FieldType:ty, )* } } => {
        #[derive(Debug)]
        pub struct $T {
            $( pub $field_name: $FieldType, )*
        }

        impl Parse for $T {
            fn parse(token_stream: &mut TokenStream, diagnostics: &mut Vec<Diagnostic>) -> ParseResult<Self> {
                let previous_token_stream = token_stream.clone();

                $(
                    let $field_name = <$FieldType>::parse(token_stream, diagnostics);
                    match &$field_name {
                        ParseResult::Ok(_) => {}
                        ParseResult::Mismatch => {
                            *token_stream = previous_token_stream;
                            return ParseResult::Mismatch;
                        }
                        ParseResult::Eof => return ParseResult::Eof,
                        ParseResult::Error => {}
                        ParseResult::LexError => return ParseResult::LexError,
                    }
                )*

                if let ( $( ParseResult::Ok($field_name), )* ) = ( $( $field_name, )* ) {
                    ParseResult::Ok(Self { $( $field_name, )* })
                } else {
                    ParseResult::Error
                }
            }

            fn describe() -> String {
                stringify!($T).into()
            }
        }
    };
}

macro_rules! ast_structs {
    { $( struct $T:ident { $( $tt:tt )* } )* } => { $( ast_struct! { struct $T { $( $tt )* } } )* };
}

macro_rules! ast_enum {
    { enum $T:ident { $( $Variant:ident($VariantType:ty), )* } } => {
        #[derive(Debug)]
        pub enum $T {
            $( $Variant($VariantType), )*
        }

        impl Parse for $T {
            fn parse(token_stream: &mut TokenStream, diagnostics: &mut Vec<Diagnostic>) -> ParseResult<Self> {
                $( 'current: {
                    let value = <$VariantType>::parse(token_stream, diagnostics);
                    if matches!(value, ParseResult::Mismatch) {
                        break 'current;
                    }

                    return value.map($T::$Variant);
                } )*

                ParseResult::Mismatch
            }

            fn describe() -> String {
                stringify!($T).into()
            }
        }
    };
}

macro_rules! ast_enums {
    { $( enum $T:ident { $( $tt:tt )* } )* } => { $( ast_enum! { enum $T { $( $tt )* } } )* };
}

// type Visibility = Option<ExplicitVisibility>;

// Modules

#[derive(Debug)]
pub struct Module {
    initial_whitespace: usize,
    items: Vec<Item>,
}

impl Parse for Module {
    fn parse(
        token_stream: &mut TokenStream,
        diagnostics: &mut Vec<Diagnostic>,
    ) -> ParseResult<Self> {
        assert!(token_stream.index() == 0);

        token_stream.skip_whitespace_tokens();
        let initial_whitespace = token_stream.index();

        let mut items = Vec::new();
        let mut any_error = false;

        loop {
            let previous_token_stream = token_stream.index();
            match Item::parse(token_stream, diagnostics) {
                ParseResult::Ok(value) => items.push(value),
                ParseResult::Mismatch => {
                    any_error = true;
                    skip_next_token_after_mismatch(token_stream, diagnostics);
                }
                ParseResult::Error => {
                    any_error = true;
                    assert!(
                        token_stream.index() > previous_token_stream,
                        "TokenStream should have advanced despite error"
                    )
                }
                ParseResult::LexError => return ParseResult::LexError,
                ParseResult::TreeEmpty => break,
            }
        }

        if any_error {
            ParseResult::Error
        } else {
            ParseResult::Ok(Module {
                initial_whitespace,
                items,
            })
        }
    }

    fn describe() -> String {
        "Module".to_string()
    }
}

fn skip_next_token_after_mismatch(
    token_stream: &mut TokenStream,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let start = token_stream.index();
    let token = token_stream
        .next()
        .expect("should not be eof")
        .expect("should not be lex error");
    let end = token_stream.index();
    diagnostics.push(Diagnostic::error_range(
        ParseError::UnexpectedToken(token.kind),
        start..end,
    ));
    token_stream.skip_whitespace_tokens();
}

ast_enums! {
    enum Item {
        // SubModule(SubModule),
        // UseItem(UseItem),
        // TypeDefinition(TypeDefinition),
        Function(Function),
        // ImplBlock(Block),
    }

    // enum ExplicitVisibility {
    //     Pub(Token<KW_PUB>),
    // }
}

// Sub-Modules

ast_structs! {}

ast_enums! {}

// Uses

ast_structs! {}

ast_enums! {}

// Sub-Modules

ast_structs! {}

ast_enums! {}

// Type-Definitions

ast_structs! {}

ast_enums! {}

// Functions

ast_structs! {
    struct Function {
        // pub_kw: Visibility,
        fn_kw: Token<KW_FN>,
        name: Required<Token<TK_IDENT>>,
        argument: Option<AssigneeExpression>,
        result_type: Option<ReturnType>,
        block: Required<Block>,
    }

    struct ReturnType {
        r_arrow: Token<TK_R_ARROW>,
        ty: Required<Type>,
    }

    struct LetBinding {
        let_kw: Token<KW_LET>,
        assignment: Required<Assignment>,
        semi: Required<Token<TK_SEMI>>,
    }

    struct Assignment {
        lhs: AssigneeExpression,
        eq: Token<TK_EQ>,
        rhs: Required<ValueExpression>,
    }

    struct UnaryExpression {
        op: UnaryOperator,
        expression: Required<Box<ValueExpression>>,
    }

    struct BinaryOperatorAndExpression {
        op: BinaryOperator,
        expression: Required<ValueExpression>,
    }

    struct BinaryOperation {
        op: BinaryOperator,
        expression: Required<ValueExpression>,
    }

    struct ValueExpression {
        unary_op: Vec<UnaryOperator>,
        expression: LiteralExpression,
        suffix: Vec<ExpressionSuffix>,
        binary_op: Option<Box<BinaryOperation>>,
    }

    struct DotIndex {
        dot: Token<TK_DOT>,
        index: DotIndexSuffix,
    }

    struct ExplicitTupleLiteral {
        pound: Token<TK_POUND>,
        values: Surrounded<Separated<ExplicitTupleValue, TK_COMMA, Box<ExplicitTupleValue>>, BRACE_PAREN>,
    }

    struct ExplicitTupleValue {
        // index: Token<TK_INTEGER>,
        colon: Token<TK_COLON>,
        value: Required<ValueExpression>,
    }

    struct TupleLiteral {
        pound: Token<TK_POUND>,
        values: Surrounded<Separated<ValueExpression, TK_COMMA, Box<ValueExpression>>, BRACE_PAREN>,
    }

    struct StructLiteral {
        at: Token<TK_AT>,
        values: Surrounded<Separated<StructField, TK_COMMA, Box<StructField>>, BRACE_PAREN>,
    }

    struct ExplicitStructField {
        name: Token<TK_IDENT>,
        colon: Token<TK_COLON>,
        value: ValueExpression,
    }

    struct ArrayLiteral {
        values: Surrounded<Separated<ValueExpression, TK_COMMA, Box<ValueExpression>>, BRACE_BRACK>,
    }

    struct SetLiteral {
        pound: Token<TK_POUND>,
        values: Surrounded<Separated<ValueExpression, TK_COMMA, Box<ValueExpression>>, BRACE_CURLY>,
    }

    struct MapLiteral {
        at: Token<TK_AT>,
        values: Surrounded<Separated<MapField, TK_COMMA, Box<MapField>>, BRACE_CURLY>,
    }

    struct MapField {
        key: ValueExpression,
        colon: Required<Token<TK_COLON>>,
        value: Required<ValueExpression>,
    }
}

ast_enums! {
    enum StructField {
        Shorthand(Token<TK_IDENT>),
        Explicit(ExplicitStructField),
    }

    enum DotIndexSuffix {
        NameIndex(Token<TK_IDENT>),
        // TupleIndex(Token<TK_INTEGER>),
    }

    enum ExpressionSuffix {
        FunctionCall(LiteralExpression),
        DotIndex(DotIndex),
    }

    enum LiteralExpression {
        True(Token<KW_TRUE>),
        False(Token<KW_FALSE>),
        // Integer(Token<TK_INTEGER>)
        // Float(Token<TK_FLOAT>)
        // String(Token<TK_STRING>),
        // Char(Token<TK_CHAR>),
        Name(Token<TK_IDENT>),
        Group(Surrounded<Box<ValueExpression>, BRACE_PAREN>),
        ExplicitTuple(ExplicitTupleLiteral),
        Tuple(TupleLiteral),
        Struct(StructLiteral),
        Array(ArrayLiteral),
        Set(SetLiteral),
        Map(MapLiteral),
        Block(Box<Block>),
    }

    enum AssigneeExpression {
        Discard(Token<TK_UNDERSCORE>),
        Name(Token<TK_IDENT>),
    }

    enum Type {
        Name(Token<TK_IDENT>),
    }

    enum ProperStatement {
        LetBinding(LetBinding),
    }
}

#[derive(Debug)]
struct ExpressionStatement {
    expression: ValueExpression,
    semi: Token<TK_SEMI>,
}

#[derive(Debug)]
enum Statement {
    Proper(ProperStatement),
    Expression(ExpressionStatement),
}

// Operators

ast_enums! {
    enum UnaryOperator {
        Neg(Token<TK_MINUS>),
        Not(Token<TK_NOT>),
    }

    enum BinaryOperator {
        Eq(Token<TK_EQ_EQ>),
        Ne(Token<TK_NE>),
        Gt(Token<TK_GT>),
        Lt(Token<TK_LT>),
        Ge(Token<TK_GE>),
        Le(Token<TK_LE>),
        Add(Token<TK_PLUS>),
        And(Token<TK_AND_AND>),
        Or(Token<TK_OR_OR>),
        BitAnd(Token<TK_AND>),
        BitOr(Token<TK_OR>),
        BitXor(Token<TK_CARET>),
        Div(Token<TK_SLASH>),
        Mul(Token<TK_STAR>),
        Rem(Token<TK_PERCENT>),
        Shl(Token<TK_SHL>),
        Shr(Token<TK_SHR>),
        Sub(Token<TK_MINUS>),

        Assign(Token<TK_EQ>),
        AddAssign(Token<TK_PLUS_EQ>),
        BitAndAssign(Token<TK_AND_EQ>),
        BitOrAssign(Token<TK_OR_EQ>),
        BitXorAssign(Token<TK_CARET_EQ>),
        DivAssign(Token<TK_SLASH_EQ>),
        MulAssign(Token<TK_STAR_EQ>),
        RemAssign(Token<TK_PERCENT_EQ>),
        ShlAssign(Token<TK_SHL_EQ>),
        ShrAssign(Token<TK_SHR_EQ>),
        SubAssign(Token<TK_MINUS_EQ>),
    }
}

#[derive(Debug)]
pub struct SeparatedNonEmpty<T: Parse, const SEPARATOR: TokenIndex, F: Parse = T> {
    first_value: F,
    rest: Vec<(Token<SEPARATOR>, T)>,
    last_separator: Option<Token<SEPARATOR>>,
}

impl<T: Parse, const SEPARATOR: TokenIndex, F: Parse> Parse for SeparatedNonEmpty<T, SEPARATOR, F> {
    fn parse(
        token_stream: &mut TokenStream,
        diagnostics: &mut Vec<Diagnostic>,
    ) -> ParseResult<Self> {
        let mut any_error = false;

        let first_value = loop {
            match F::parse(token_stream, diagnostics) {
                ParseResult::Ok(value) => break value,
                ParseResult::TreeEmpty => todo!(),
                ParseResult::Mismatch => todo!(),
                ParseResult::Error => return ParseResult::Error,
                ParseResult::LexError => return ParseResult::LexError,
            }
        };

        let mut rest = Vec::new();

        let last_separator = loop {
            let previous_token_stream = token_stream.index();

            let separator = match Token::<SEPARATOR>::parse(token_stream, diagnostics) {
                ParseResult::Ok(separator) => Some(separator),
                ParseResult::TreeEmpty => todo!(),
                ParseResult::Mismatch => todo!(),
                ParseResult::Error => panic!("separator should not error"),
                ParseResult::LexError => return ParseResult::LexError,
            };

            let value = match T::parse(token_stream, diagnostics) {
                ParseResult::Ok(value) => Some(value),
                ParseResult::TreeEmpty => todo!(),
                ParseResult::Mismatch => todo!(),
                ParseResult::Error => {
                    any_error = true;
                    continue;
                }
                ParseResult::LexError => return ParseResult::LexError,
            };

            if previous_token_stream == token_stream.index() {
                any_error = true;
                skip_next_token_after_mismatch(token_stream, diagnostics);
            }

            let Some(separator) = separator else {
                any_error = true;
                diagnostics.push(Diagnostic::error(
                    ParseError::Expected(Token::<TK_COMMA>::describe()),
                    token_stream.index(),
                ));
                continue;
            };
            let Some(value) = value else {
                any_error = true;
                diagnostics.push(Diagnostic::error(
                    ParseError::Expected(T::describe()),
                    token_stream.index(),
                ));
                continue;
            };

            if !any_error {
                rest.push((separator, value));
            }
        };

        if any_error {
            ParseResult::Error
        } else {
            ParseResult::Ok(Self {
                first_value,
                rest,
                last_separator,
            })
        }
    }

    fn describe() -> String {
        format!(
            "{} separated list of {}",
            Token::<SEPARATOR>::describe(),
            T::describe()
        )
    }
}

type Separated<T, const SEPARATOR: TokenIndex, F = T> = Option<SeparatedNonEmpty<T, SEPARATOR, F>>;

type Block = Surrounded<BlockContent, BRACE_CURLY>;

#[derive(Debug)]
pub struct BlockContent {
    statements: Vec<Statement>,
    last_expression: Option<ValueExpression>,
}

impl Parse for BlockContent {
    fn parse(
        token_stream: &mut TokenStream,
        diagnostics: &mut Vec<Diagnostic>,
    ) -> ParseResult<Self> {
        let mut statements = vec![];
        let mut last_expression = None;

        let mut any_error = false;

        loop {
            match ValueExpression::parse(token_stream, diagnostics) {
                ParseResult::Ok(expression) => {
                    match Token::<TK_SEMI>::parse(token_stream, diagnostics) {
                        ParseResult::Ok(semi) => {
                            statements.push(Statement::Expression(ExpressionStatement {
                                expression,
                                semi,
                            }));
                            continue;
                        }
                        ParseResult::Mismatch => {
                            last_expression = Some(expression);
                            continue;
                        }
                        ParseResult::TreeEmpty => todo!(),
                        ParseResult::Error => return ParseResult::Error,
                        ParseResult::LexError => return ParseResult::LexError,
                    }
                }
                ParseResult::Mismatch => {}
                ParseResult::TreeEmpty => todo!(),
                ParseResult::Error => return ParseResult::Error,
                ParseResult::LexError => return ParseResult::LexError,
            }

            match ProperStatement::parse(token_stream, diagnostics) {
                ParseResult::Ok(statement) => statements.push(Statement::Proper(statement)),
                ParseResult::Mismatch => {
                    any_error = true;
                    skip_next_token_after_mismatch(token_stream, diagnostics);
                }
                ParseResult::TreeEmpty => todo!(),
                ParseResult::Error => return ParseResult::Error,
                ParseResult::LexError => return ParseResult::LexError,
            }
        }
    }

    fn describe() -> String {
        "Block".into()
    }
}
