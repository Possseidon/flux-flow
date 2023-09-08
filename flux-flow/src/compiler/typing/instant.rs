/// A type that can represent an instant in time.
///
/// For now this does not store any additional information about the time, but in the future it
/// should be possible for the compiler to order different instant types in a way, where it can
/// make assumptions about their order. This can be done by inspecting the flow-graph of functions.
#[derive(Default)]
pub(crate) enum InstantType {
    #[default]
    Never,
    WIP,
}
