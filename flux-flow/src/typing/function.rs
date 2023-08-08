use super::Type;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionType {
    arg: Type,
    ret: Type,
}
