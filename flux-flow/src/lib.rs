pub mod compiler;
pub mod stack;
pub mod typing;
pub mod value;

// TODO: Move somewhere appropriate

use std::sync::Arc;

#[derive(Clone, Debug, PartialEq, Eq)]
struct Path {
    segments: Arc<[String]>, // this would greatly benefit from SmartString since segments are generally very short
}
