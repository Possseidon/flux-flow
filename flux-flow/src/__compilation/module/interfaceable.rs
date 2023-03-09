pub enum Interfaceable<T> {
    Public(T),
    Private(T),
}

impl<T> Interfaceable<T> {
    pub fn as_inner(&self) -> &T {
        match self {
            Self::Public(interface) | Self::Private(interface) => interface,
        }
    }

    pub fn into_inner(self) -> T {
        match self {
            Self::Public(interface) | Self::Private(interface) => interface,
        }
    }

    pub fn is_public(&self) -> bool {
        matches!(self, Self::Public(..))
    }

    pub fn as_public(&self) -> Option<&T> {
        if let Self::Public(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn try_into_public(self) -> Result<T, Self> {
        if let Self::Public(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    pub fn is_private(&self) -> bool {
        matches!(self, Self::Private(..))
    }

    pub fn as_private(&self) -> Option<&T> {
        if let Self::Private(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn try_into_private(self) -> Result<T, Self> {
        if let Self::Private(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}
