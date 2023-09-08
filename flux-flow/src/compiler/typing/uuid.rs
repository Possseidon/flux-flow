use std::collections::BTreeSet;

use std::sync::Arc;

/// The type of a UUID, limited by either a fixed set of values or types of generated UUIDs.
///
/// [`UuidType::Generated`] with all `false` shall be used as the `never` type.
pub(crate) enum UuidType {
    Generated(GeneratedUuidType),
    Values(Arc<BTreeSet<u128>>), // TODO: use uuid crate
}

impl Default for UuidType {
    fn default() -> Self {
        Self::Generated(Default::default())
    }
}

/// The type of a randomly generated UUID.
///
/// Also supports `nil` and `max`, even though those are technically not results you can get when
/// randomly generating UUIDs.
#[derive(Default)]
pub(crate) struct GeneratedUuidType {
    pub(crate) nil: bool,
    pub(crate) mac: bool,
    pub(crate) dce: bool,
    pub(crate) md5: bool,
    pub(crate) random: bool,
    pub(crate) sha1: bool,
    pub(crate) sort_mac: bool,
    pub(crate) sort_rand: bool,
    pub(crate) custom: bool,
    pub(crate) max: bool,
}
