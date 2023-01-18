use super::{
    parser::{Descriptor, Parse, ParseResult, Required},
    token_stream::{tokens::*, TokenIndex, TokenKind, TokenStream},
};

// /// Panics when attempting to parse.
// #[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
// struct Todo;

// impl Parse for Todo {
//     fn parse(_ts: TokenStream) -> ParseResult<Self> {
//         todo!()
//     }
// }

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token<const TK: TokenIndex> {
    position: usize,
    len: usize,
}

impl<const TK: TokenIndex> Parse for Token<TK> {
    fn parse(ts: TokenStream) -> ParseResult<Self> {
        match ts.next_non_whitespace_token() {
            Some(token) => match token {
                Ok((token, ts)) => {
                    if token.kind as TokenIndex == TK {
                        ParseResult::ok(
                            Token {
                                position: token.position,
                                len: token.len,
                            },
                            ts,
                        )
                    } else {
                        ParseResult::default()
                    }
                }
                Err(error) => ParseResult::err(vec![error.into()]),
            },
            None => ParseResult::default(),
        }
    }

    fn descriptor() -> Descriptor {
        Descriptor::Token(TK.try_into().unwrap())
    }
}

macro_rules! ast_struct {
    { struct $T:ident { $( $field_name:ident: $FieldType:ty, )* } } => {
        #[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
        pub struct $T {
            $( $field_name: $FieldType, )*
        }

        impl Parse for $T {
            fn parse(mut ts: TokenStream) -> ParseResult<Self> {
                let mut errors = Vec::new();
                $(
                    let mut $field_name = <$FieldType>::parse(ts);
                    errors.append(&mut $field_name.errors);
                    if let Some((_, updated_ts)) = $field_name.result {
                        ts = updated_ts;
                    }
                )*
                match ( $( $field_name.result, )* ) {
                    ( $( Some(($field_name, _)), )* ) => ParseResult::warn(
                        Self {
                            $( $field_name, )*
                        },
                        ts,
                        errors,
                    ),
                    _ => ParseResult::err(errors),
                }
            }

            fn descriptor() -> Descriptor {
                Descriptor::Named(stringify!($T))
            }
        }
    };
}

macro_rules! ast_structs {
    { $( struct $T:ident { $( $tt:tt )* } )* } => { $( ast_struct! { struct $T { $( $tt )* } } )* };
}

// TODO: Put Required<> in more places
// TODO: Sort structs

ast_structs! {
    struct Function {
        pub_kw: Option<Token<KW_PUB>>,
        fn_kw: Token<KW_FN>,
        name: Required<Token<TK_IDENT>>,
        signature: Required<Pattern>,
        return_type: Option<ReturnType>,
        block: Required<Block>,
    }

    struct Lambda {
        signature: LambdaSignature,
        body: Expression,
    }

    struct Block {
        lcurly: Token<TK_L_CURLY>,
        statement: Vec<Statement>,
        last_expression: Option<Box<Expression>>,
        rcurly: Required<Token<TK_R_CURLY>>,
    }

    struct ExpressionStatement {
        expression: Expression,
        semi: Token<TK_SEMI>,
    }

    struct ReturnType {
        rarrow: Token<TK_R_ARROW>,
        ty: Type,
    }

    struct LetBinding {
        let_kw: Token<KW_LET>,
        pattern: Pattern,
        eq: Token<TK_EQ>,
        expression: Expression,
        semi: Token<TK_SEMI>,
    }

    struct Assignment {
        // l_value: LValue,
        eq: Token<TK_EQ>,
        expression: Expression,
    }

    struct TuplePattern {
        l_paren: Token<TK_L_PAREN>,
        r_paren: Required<Token<TK_R_PAREN>>,
    }

    struct StructPattern {
        l_curly: Token<TK_L_CURLY>,
        r_curly: Required<Token<TK_R_CURLY>>,
    }

    struct RuntimeType {
        name: Token<TK_IDENT>, // TODO: RuntimeType is more than just an ident
    }

    struct OrType {
        or: Token<TK_OR>,
        ty: RuntimeType,
    }

    struct UnionType {
        first: RuntimeType,
        rest: Vec<OrType>,
    }

    struct TypeMaybeComma {
        ty: Type,
        comma: Option<Token<TK_COMMA>>,
    }

    struct TupleTypeList {
        first: Type,
        comma: Required<Token<TK_COMMA>>,
        rest: Vec<TypeMaybeComma>,
    }

    struct TupleType {
        l_paren: Token<TK_L_PAREN>,
        types: Option<TupleTypeList>,
        r_paren: Required<Token<TK_R_PAREN>>,
    }

    struct ExpressionMaybeComma {
        expression: Expression,
        comma: Option<Token<TK_COMMA>>,
    }

    struct TupleExpressionList {
        first: Expression,
        comma: Required<Token<TK_COMMA>>,
        rest: Vec<ExpressionMaybeComma>,
    }

    struct TupleLiteral {
        l_paren: Token<TK_L_PAREN>,
        types: Option<TupleExpressionList>,
        r_paren: Required<Token<TK_R_PAREN>>,
    }

    struct LambdaSignaturePattern {
        l_pipe: Token<TK_OR>,
        pattern: Pattern,
        r_pipe: Token<TK_OR>,
    }

    struct ColonType {
        colon: Token<TK_COLON>,
        ty: Type,
    }

    struct NamePattern {
        name: Token<TK_IDENT>,
        ty: Option<ColonType>,
    }

    struct LabeledBlock {
        label: Option<Token<TK_LABEL>>,
        block: Block,
    }

    struct InfiniteLoopExpression {
        label: Option<Token<TK_LABEL>>,
        loop_kw: Token<KW_LOOP>,
        block: Box<Block>,
    }

    struct WhileLoopExpression {
        while_kw: Token<KW_WHILE>,
        condition: Required<Expression>,
        body: Required<Block>,
    }

    struct WhileLetLoopExpression {
        while_kw: Token<KW_WHILE>,
        // TODO: Improve to allow multiple bindings and stuff
        binding: LetBinding,
        body: Required<Block>,
    }

    struct ForLoopExpression {
        for_kw: Token<KW_FOR>,
        pattern: Required<Pattern>,
        in_kw: Required<Token<KW_IN>>,
        iterator: Required<Expression>,
        body: Required<Block>,
    }

    struct IfExpression {
        if_kw: Token<KW_IF>,
        condition: Required<Expression>,
        body: Required<Block>,
        else_if_expressions: Vec<ElseIfExpression>,
        else_expression: Option<ElseExpression>,
    }

    struct ElseIfExpression {
        else_kw: Token<KW_ELSE>,
        if_expression: IfExpression,
    }

    struct ElseExpression {
        else_kw: Token<KW_ELSE>,
        body: Required<Block>,
    }

    struct IfLetExpression {
        if_kw: Token<KW_IF>,
        binding: LetBinding,
        body: Required<Block>,
    }

    struct MatchCondition {
        if_kw: Token<KW_IF>,
        expression: Expression,
    }

    struct MatchArm {
        pattern: Pattern,
        match_condition: Option<MatchCondition>,
        r_arrow: Token<TK_FAT_ARROW>,
        expression: Expression,
    }

    struct MatchBlock {
        l_curly: Token<TK_L_CURLY>,
        arms: Vec<MatchArm>,
        r_curly: Token<TK_R_CURLY>,
    }

    struct MatchExpression {
        match_kw: Token<KW_MATCH>,
        expression: Expression,
        match_block: MatchBlock,
    }

    struct ReturnStatement {
        return_kw: Token<KW_RETURN>,
        expression: Option<Expression>,
        semi: Token<TK_SEMI>,
    }

    struct ContinueStatement {
        continue_kw: Token<KW_CONTINUE>,
        semi: Token<TK_SEMI>,
    }

    struct BreakStatement {
        break_kw: Token<KW_BREAK>,
        expression: Option<Expression>,
        semi: Token<TK_SEMI>,
    }

    struct AssigneeExpression {}
    struct ValueExpression {}
    struct PlaceExpression {}
}

macro_rules! ast_enum {
    { enum $T:ident { $( $Variant:ident($VariantType:ty), )* } } => {
        #[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
        pub enum $T {
            $( $Variant($VariantType), )*
        }

        impl Parse for $T {
            fn parse(ts: TokenStream) -> ParseResult<Self> {
                let mut errors = Vec::new();
                $(
                    let mut result = <$VariantType>::parse(ts);
                    if let Some((variant, ts)) = result.result {
                        return ParseResult::warn($T::$Variant(variant), ts, result.errors);
                    } else {
                        errors.append(&mut result.errors);
                    }
                )*
                ParseResult::err(errors)
            }

            fn descriptor() -> Descriptor {
                Descriptor::Named(stringify!($T))
            }
        }
    };
}

macro_rules! ast_enums {
    { $( enum $T:ident { $( $tt:tt )* } )* } => { $( ast_enum! { enum $T { $( $tt )* } } )* };
}

// TODO: Sort enum variants
// TODO: Sort enum types

ast_enums! {
    // TODO: Implement this one manually to deal with blocks without semicolon
    enum Statement {
        Expression(ExpressionStatement),
        Block(LabeledBlock),
        InfiniteLoop(InfiniteLoopExpression),
        WhileLetLoop(WhileLetLoopExpression),
        WhileLoop(WhileLoopExpression),
        ForLoop(ForLoopExpression),
        IfLet(IfLetExpression),
        If(IfExpression),
        Match(MatchExpression),
        LetBinding(LetBinding),
        Assignment(Assignment),
        Return(ReturnStatement),
        Break(BreakStatement),
        Continue(ContinueStatement),
    }

    enum Expression {
        Block(LabeledBlock),
        InfiniteLoop(InfiniteLoopExpression),
        WhileLetLoop(WhileLetLoopExpression),
        WhileLoop(WhileLoopExpression),
        ForLoop(ForLoopExpression),
        IfLet(IfLetExpression),
        If(IfExpression),
        Match(MatchExpression),
    }

    enum Pattern {
        Placeholder(Token<TK_UNDERSCORE>),
        Name(NamePattern),
        Tuple(TuplePattern),
        Struct(StructPattern),
    }

    enum LambdaSignature {
        Empty(Token<TK_OR_OR>),
        Pattern(LambdaSignaturePattern),
    }

    enum Type {
        Inferred(Token<TK_UNDERSCORE>),
        Never(Token<TK_NOT>),
        Union(UnionType),
    }
}
