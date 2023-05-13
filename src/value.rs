use core::fmt::{self, Write};
use crate::{Result, QbeError};

pub(crate) trait QbeCodegen<Writer: Write> {
    fn gen(&self, dest: &mut Writer) -> fmt::Result;
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
    pub(crate) fn basic_name(&self) -> &'static str {
        match self {
            Self::Word           => "w",
            Self::Long           => "l",
            Self::Single         => "s",
            Self::Double         => "d",
            Self::Byte           => "b",
            Self::Half           => "h",
            Self::SignedByte     => "b",
            Self::SignedHalf     => "h",
            Self::UserDefined(_) => "l",
        }
    }
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
    pub fn is_integer(&self) -> bool {
        !matches!(self, Self::Single | Self::Double | Self::UserDefined(_))
    }
    pub fn is_numeric(&self) -> bool {
        !matches!(self, Self::UserDefined(_))
    }
    pub fn is_floating(&self) -> bool {
        matches!(self, Self::Single | Self::Double)
    }
    pub fn is_pointer(&self) -> bool {
        matches!(self, Self::Long | Self::UserDefined(_))
    }
}
impl<W: Write> QbeCodegen<W> for QbeType {
    fn gen(&self, d: &mut W) -> fmt::Result {
        match self {
            Self::Word            => d.write_str("w"),
            Self::Long            => d.write_str("l"),
            Self::Single          => d.write_str("s"),
            Self::Double          => d.write_str("d"),
            Self::Byte            => d.write_str("ub"),
            Self::Half            => d.write_str("uh"),
            Self::SignedByte      => d.write_str("sb"),
            Self::SignedHalf      => d.write_str("sh"),
            Self::UserDefined(id) => write!(d, ":_{}", id),
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
impl<W: Write> QbeCodegen<W> for QbeData<'_> {
    fn gen(&self, d: &mut W) -> fmt::Result {
        match self {
            Self::String(s)                 => write!(d, "\"{}\"", s.escape_default()),
            Self::Global(id)                => write!(d, "$_{}", id),
            Self::OffsetGlobal(id, offset)  => write!(d, "$_{}+{}", id, offset),
            Self::Constant(typ, val)        => write!(d, "{} {}", typ.basic_name(), val),
            Self::Named(name)               => write!(d, "${}", name),
            Self::OffsetNamed(name, offset) => write!(d, "{}+{}", name, offset),
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
impl<'a> From<QbeValue<'a>> for QbeData<'a> {
    fn from(item: QbeValue<'a>) -> Self {
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

// Note that it is a logical error (though not a compile- or runtime- error) to
// use a `QbeLabel` or `QbeValue` with a `QbeContext`/`QbeFunctionBuilder` other
// than the one that created it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct QbeLabel(pub(crate) u32);
impl<W: Write> QbeCodegen<W> for QbeLabel {
    fn gen(&self, d: &mut W) -> fmt::Result {
        write!(d, "@_{}", self.0)
    }
}

// Note that it is a logical error (though not a compile- or runtime- error) to
// use a `QbeLabel` or `QbeValue` with a `QbeContext`/`QbeFunctionBuilder` other
// than the one that created it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum QbeValue<'a> {
    Global(u32),
    Temporary(QbeType, u32),
    Constant(QbeType, u64),
    Named(&'a str),
    ThreadLocalNamed(&'a str),
}
impl<'a> QbeValue<'a> {
    pub fn type_of(&self) -> QbeType {
        match self {
            Self::Global(_)           => QbeType::Long,
            Self::Temporary(typ, _)   => *typ,
            Self::Constant(typ, _)    => *typ,
            Self::Named(_)            => QbeType::Long,
            Self::ThreadLocalNamed(_) => QbeType::Long,
        }
    }
    pub fn common_type(&self, with: &'a QbeValue) -> Result<QbeType> {
        let typ = match self {
            Self::Global(_)           => QbeType::Long,
            Self::Temporary(typ, _)   => *typ,
            Self::Constant(_, _)      => return Ok(with.type_of()),
            Self::Named(_)            => QbeType::Long,
            Self::ThreadLocalNamed(_) => QbeType::Long,
        }.promote();
        let other = match with {
            Self::Global(_)           => QbeType::Long,
            Self::Temporary(typ, _)   => *typ,
            Self::Constant(_, _)      => return Ok(typ),
            Self::Named(_)            => QbeType::Long,
            Self::ThreadLocalNamed(_) => QbeType::Long,
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
        matches!(self, Self::Global(_) | Self::Named(_) | Self::ThreadLocalNamed(_))
    }
}
impl<W: Write> QbeCodegen<W> for QbeValue<'_> {
    fn gen(&self, d: &mut W) -> fmt::Result {
        match self {
            Self::Global(id)             => write!(d, "$_{}", id),
            Self::Temporary(_, id)       => write!(d, "%_{}", id),
            Self::Constant(_, val)       => write!(d, "{}", val),
            Self::Named(name)            => write!(d, "${}", name),
            Self::ThreadLocalNamed(name) => write!(d, "thread ${}", name),
        }
    }
}
impl From<u8> for QbeValue<'_> {
    fn from(item: u8) -> Self {
        // automatic promotion from byte to word
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<u16> for QbeValue<'_> {
    fn from(item: u16) -> Self {
        // automatic promotion from half to word
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<u32> for QbeValue<'_> {
    fn from(item: u32) -> Self {
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<u64> for QbeValue<'_> {
    fn from(item: u64) -> Self {
        Self::Constant(QbeType::Long, item)
    }
}
impl From<i8> for QbeValue<'_> {
    fn from(item: i8) -> Self {
        // automatic promotion from byte to word
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<i16> for QbeValue<'_> {
    fn from(item: i16) -> Self {
        // automatic promotion from half to word
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<i32> for QbeValue<'_> {
    fn from(item: i32) -> Self {
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<i64> for QbeValue<'_> {
    fn from(item: i64) -> Self {
        Self::Constant(QbeType::Long, item as u64)
    }
}
impl From<f32> for QbeValue<'_> {
    fn from(item: f32) -> Self {
        Self::Constant(QbeType::Single, item.to_bits() as u64)
    }
}
impl From<f64> for QbeValue<'_> {
    fn from(item: f64) -> Self {
        Self::Constant(QbeType::Double, item.to_bits())
    }
}
impl<'a> From<&'a str> for QbeValue<'a> {
    fn from(item: &'a str) -> Self {
        Self::Named(item)
    }
}

// for ease of design, a forward declaration cannot use `export_as`
// if this is desired, use a global named symbol instead
// doesn't derive `Copy` or `Clone` to disallow redeclaring a symbol
#[derive(Debug, PartialEq, Eq)]
pub struct QbeForwardDecl(pub(crate) u32);
impl From<&QbeForwardDecl> for QbeValue<'_> {
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
