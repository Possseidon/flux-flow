use super::Type;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionType {
    arg_type: Type,
    ret_type: Type,
}
