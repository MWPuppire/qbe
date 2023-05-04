use std::fmt;
use crate::{Result, QbeError};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum QbeBasicType {
    Word,
    Long,
    Single,
    Double,
    Byte,
    Half,
}
impl QbeBasicType {
    pub(crate) fn promote(&self) -> QbeBasicType {
        match self {
            Self::Byte => Self::Word,
            Self::Half => Self::Word,
            x => *x,
        }
    }
}
impl fmt::Display for QbeBasicType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Word   => write!(f, "w"),
            Self::Long   => write!(f, "l"),
            Self::Single => write!(f, "s"),
            Self::Double => write!(f, "d"),
            Self::Byte   => write!(f, "b"),
            Self::Half   => write!(f, "h"),
        }
    }
}
impl TryFrom<QbeType> for QbeBasicType {
    type Error = QbeError;
    fn try_from(item: QbeType) -> Result<Self> {
        match item {
            QbeType::Word => Ok(Self::Word),
            QbeType::Long => Ok(Self::Long),
            QbeType::Single => Ok(Self::Single),
            QbeType::Double => Ok(Self::Double),
            QbeType::Byte => Ok(Self::Byte),
            QbeType::Half => Ok(Self::Half),
            QbeType::SignedByte => Ok(Self::Byte),
            QbeType::SignedHalf => Ok(Self::Half),
            QbeType::UserDefined(_) => Err(QbeError::NotBasic),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum QbeType {
    Word,
    Long,
    Single,
    Double,
    Byte,
    Half,
    SignedByte,
    SignedHalf,
    UserDefined(u32),
}
impl QbeType {
    pub(crate) fn promote(&self) -> QbeType {
        match self {
            Self::Byte => Self::Word,
            Self::Half => Self::Word,
            Self::SignedByte => Self::Word,
            Self::SignedHalf => Self::Word,
            x => *x,
        }
    }
    pub(crate) fn pointer_ud(&self) -> QbeType {
        match self {
            Self::UserDefined(_) => Self::Long,
            x => *x,
        }
    }
    pub(crate) fn cast(&self) -> QbeType {
        match self {
            Self::Word => Self::Single,
            Self::Long => Self::Double,
            Self::Single => Self::Word,
            Self::Double => Self::Long,
            x => *x,
        }
    }
    pub(crate) fn is_word(&self) -> bool {
        *self == Self::Word
    }
    pub(crate) fn is_long(&self) -> bool {
        *self == Self::Long
    }
    pub(crate) fn is_single(&self) -> bool {
        *self == Self::Single
    }
    pub(crate) fn is_double(&self) -> bool {
        *self == Self::Double
    }
    pub(crate) fn is_any(&self) -> bool {
        true
    }
    pub(crate) fn as_basic(&self) -> Result<QbeBasicType> {
        (*self).try_into()
    }
    pub fn is_integer(&self) -> bool {
        match self {
            Self::Single => false,
            Self::Double => false,
            Self::UserDefined(_) => false,
            _ => true,
        }
    }
    pub fn is_numeric(&self) -> bool {
        match self {
            Self::UserDefined(_) => false,
            _ => true,
        }
    }
    pub fn is_floating(&self) -> bool {
        match self {
            Self::Single => true,
            Self::Double => true,
            _ => false,
        }
    }
    pub fn is_pointer(&self) -> bool {
        match self {
            Self::Long => true,
            Self::UserDefined(_) => true,
            _ => false,
        }
    }
}
impl From<QbeBasicType> for QbeType {
    fn from(item: QbeBasicType) -> Self {
        match item {
            QbeBasicType::Word   => Self::Word,
            QbeBasicType::Long   => Self::Long,
            QbeBasicType::Single => Self::Single,
            QbeBasicType::Double => Self::Double,
            QbeBasicType::Byte   => Self::Byte,
            QbeBasicType::Half   => Self::Half,
        }
    }
}
impl fmt::Display for QbeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Word            => write!(f, "w"),
            Self::Long            => write!(f, "l"),
            Self::Single          => write!(f, "s"),
            Self::Double          => write!(f, "d"),
            Self::Byte            => write!(f, "ub"),
            Self::Half            => write!(f, "uh"),
            Self::SignedByte      => write!(f, "sb"),
            Self::SignedHalf      => write!(f, "sh"),
            Self::UserDefined(id) => write!(f, ":_{}", id),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum QbeData<'a> {
    String(&'a str),
    Global(u32),
    OffsetGlobal(u32, u64),
    Constant(QbeType, u64),
    Named(&'a str),
    OffsetNamed(&'a str, u64),
}
impl<'a> fmt::Display for QbeData<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s)                 => write!(f, "\"{}\"", s.escape_default()),
            Self::Global(id)                => write!(f, "$_{}", id),
            Self::OffsetGlobal(id, offset)  => write!(f, "$_{}+{}", id, offset),
            Self::Constant(typ, val)        => write!(f, "{} {}", unsafe {
                typ.as_basic().unwrap_unchecked()
            }, val),
            Self::Named(name)               => name.fmt(f),
            Self::OffsetNamed(name, offset) => write!(f, "{}+{}", name, offset),
        }
    }
}
impl From<u8> for QbeData<'_> {
    fn from(item: u8) -> Self {
        Self::Constant(QbeType::Byte, item as u64)
    }
}
impl From<u16> for QbeData<'_> {
    fn from(item: u16) -> Self {
        Self::Constant(QbeType::Half, item as u64)
    }
}
impl From<u32> for QbeData<'_> {
    fn from(item: u32) -> Self {
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<u64> for QbeData<'_> {
    fn from(item: u64) -> Self {
        Self::Constant(QbeType::Long, item)
    }
}
impl From<i8> for QbeData<'_> {
    fn from(item: i8) -> Self {
        Self::Constant(QbeType::Byte, item as u64)
    }
}
impl From<i16> for QbeData<'_> {
    fn from(item: i16) -> Self {
        Self::Constant(QbeType::Half, item as u64)
    }
}
impl From<i32> for QbeData<'_> {
    fn from(item: i32) -> Self {
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<i64> for QbeData<'_> {
    fn from(item: i64) -> Self {
        Self::Constant(QbeType::Long, item as u64)
    }
}
impl From<f32> for QbeData<'_> {
    fn from(item: f32) -> Self {
        Self::Constant(QbeType::Word, item.to_bits() as u64)
    }
}
impl From<f64> for QbeData<'_> {
    fn from(item: f64) -> Self {
        Self::Constant(QbeType::Long, item.to_bits())
    }
}
impl<'a> From<&'a str> for QbeData<'a> {
    fn from(item: &'a str) -> Self {
        Self::String(item)
    }
}
impl From<QbeValue> for QbeData<'_> {
    fn from(item: QbeValue) -> Self {
        match item {
            QbeValue::Global(id)         => Self::Global(id),
            QbeValue::Named(name)        => Self::Named(name),
            _ => panic!("cannot use one of those"),
        }
    }
}
impl From<&QbeForwardDecl> for QbeData<'_> {
    fn from(item: &QbeForwardDecl) -> Self {
        Self::Global(item.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct QbeLabel(pub(crate) u32);
impl fmt::Display for QbeLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@_{}", self.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum QbeValue {
    Global(u32),
    Temporary(QbeType, u32),
    Constant(QbeType, u64),
    Named(&'static str),
}
impl QbeValue {
    pub fn type_of(&self) -> QbeType {
        match self {
            Self::Global(_)         => QbeType::Long,
            Self::Temporary(typ, _) => *typ,
            Self::Constant(typ, _)  => *typ,
            Self::Named(_)          => QbeType::Long,
        }
    }
    pub fn common_type(&self, with: &QbeValue) -> Result<QbeType> {
        let typ = match self {
            Self::Global(_)         => QbeType::Long,
            Self::Temporary(typ, _) => *typ,
            Self::Constant(_, _)    => return Ok(with.type_of()),
            Self::Named(_)          => QbeType::Long,
        }.promote();
        let other = match with {
            Self::Global(_)         => QbeType::Long,
            Self::Temporary(typ, _) => *typ,
            Self::Constant(_, _)    => return Ok(typ),
            Self::Named(_)          => QbeType::Long,
        }.promote();
        if typ == other {
            return Ok(typ);
        }
        match (typ, other) {
            (QbeType::Word, QbeType::Long) => Ok(QbeType::Word),
            (QbeType::Long, QbeType::Word) => Ok(QbeType::Word),
            _ => Err(QbeError::CannotInferType),
        }
    }
    pub(crate) fn is_global(&self) -> bool {
        // how the code is currently written, only global symbols can be `Named`
        // if this changes, this function will need to change accordingly
        match self {
            Self::Global(_) => true,
            Self::Named(_) => true,
            _ => false,
        }
    }
}
impl fmt::Display for QbeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Global(id)         => write!(f, "$_{}", id),
            Self::Temporary(_, id)   => write!(f, "%_{}", id),
            Self::Constant(_, val)   => val.fmt(f),
            Self::Named(name)        => name.fmt(f),
        }
    }
}
impl From<u8> for QbeValue {
    fn from(item: u8) -> Self {
        // automatic promotion from byte to word
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<u16> for QbeValue {
    fn from(item: u16) -> Self {
        // automatic promotion from half to word
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<u32> for QbeValue {
    fn from(item: u32) -> Self {
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<u64> for QbeValue {
    fn from(item: u64) -> Self {
        Self::Constant(QbeType::Long, item)
    }
}
impl From<i8> for QbeValue {
    fn from(item: i8) -> Self {
        // automatic promotion from byte to word
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<i16> for QbeValue {
    fn from(item: i16) -> Self {
        // automatic promotion from half to word
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<i32> for QbeValue {
    fn from(item: i32) -> Self {
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<i64> for QbeValue {
    fn from(item: i64) -> Self {
        Self::Constant(QbeType::Long, item as u64)
    }
}
impl From<f32> for QbeValue {
    fn from(item: f32) -> Self {
        Self::Constant(QbeType::Single, item.to_bits() as u64)
    }
}
impl From<f64> for QbeValue {
    fn from(item: f64) -> Self {
        Self::Constant(QbeType::Double, item.to_bits())
    }
}
impl From<&'static str> for QbeValue {
    fn from(item: &'static str) -> Self {
        Self::Named(item)
    }
}

// for ease of design, a forward declaration cannot use `export_as`
// if this is desired, use a global named symbol instead
// doesn't derive `Copy` or `Clone` to disallow redeclaring a symbol
#[derive(Debug, PartialEq, Eq)]
pub struct QbeForwardDecl(pub(crate) u32);
impl From<&QbeForwardDecl> for QbeValue {
    fn from(item: &QbeForwardDecl) -> Self {
        Self::Global(item.0)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct QbeForwardLabel(pub(crate) u32);
impl From<&QbeForwardLabel> for QbeLabel {
    fn from(item: &QbeForwardLabel) -> Self {
        QbeLabel(item.0)
    }
}
