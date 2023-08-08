#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UuidType {
    /// A type that can hold any UUID that matches the mask.
    Mask(UuidMask),
    /// A type that can hold the specified UUID.
    Uuid(u128), // TODO: use UUID crate
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UuidMask {
    nil: bool,
    mac: bool,
    dce: bool,
    md5: bool,
    random: bool,
    sha1: bool,
    sort_mac: bool,
    sort_rand: bool,
    custom: bool,
    max: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UuidVersion {
    Mac,
    Dce,
    Md5,
    Random,
    Sha1,
    SortMac,
    SortRand,
    Custom,
}
