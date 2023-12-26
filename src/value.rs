use crate::func::{QbeFunctionBuilder, QbeFunctionCall, QbeVariadicFunctionCall};
use crate::{QbeError, Result};
use std::fmt::{self, Write};

pub(crate) trait QbeCodegen<Writer: Write> {
    fn gen(&self, dest: &mut Writer) -> fmt::Result;
}

/// An enumeration of QBE types. `UserDefined` types shouldn't be created
/// manually, only by the library.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum QbeType {
    /// The `w` type in QBE, representing 32-bit integers.
    Word,
    /// The `l` type in QBE, representing 64-bit integers.
    Long,
    /// The `s` type in QBE, representing 32-bit floating-points.
    Single,
    /// The `d` type in QBE, representing 64-bit floating-points.
    Double,
    /// The `b` or `ub` type in QBE, representing 8-bit integers.
    Byte,
    /// The `h` or `uh` type in QBE, representing 16-bit integers.
    Half,
    /// The `sb` type in QBE, representing 8-bit signed integers.
    SignedByte,
    /// The `sh` type in QBE, representing 16-bit signed integers.
    SignedHalf,
    /// Any user-defined aggregate types created by the program.
    UserDefined(u32),
}
impl QbeType {
    #[inline]
    pub(crate) fn basic_name(&self) -> &'static str {
        match self {
            Self::Word => "w",
            Self::Long => "l",
            Self::Single => "s",
            Self::Double => "d",
            Self::Byte => "b",
            Self::Half => "h",
            Self::SignedByte => "b",
            Self::SignedHalf => "h",
            Self::UserDefined(_) => "l",
        }
    }
    #[inline]
    pub(crate) fn promote(&self) -> QbeType {
        match self {
            Self::Byte => Self::Word,
            Self::Half => Self::Word,
            Self::SignedByte => Self::Word,
            Self::SignedHalf => Self::Word,
            x => *x,
        }
    }
    #[inline]
    pub(crate) fn pointer_ud(&self) -> QbeType {
        match self {
            Self::UserDefined(_) => Self::Long,
            x => *x,
        }
    }
    #[inline]
    pub(crate) fn cast(&self) -> QbeType {
        match self {
            Self::Word => Self::Single,
            Self::Long => Self::Double,
            Self::Single => Self::Word,
            Self::Double => Self::Long,
            x => *x,
        }
    }
    #[inline]
    pub(crate) fn is_word(&self) -> bool {
        *self == Self::Word
    }
    #[inline]
    pub(crate) fn is_long(&self) -> bool {
        *self == Self::Long
    }
    #[inline]
    pub(crate) fn is_single(&self) -> bool {
        *self == Self::Single
    }
    #[inline]
    pub(crate) fn is_double(&self) -> bool {
        *self == Self::Double
    }
    #[inline]
    pub(crate) fn is_any(&self) -> bool {
        true
    }

    /// Tests if the type is a basic integer type (not floating-points or
    /// aggregates).
    #[inline]
    pub fn is_integer(&self) -> bool {
        !matches!(self, Self::Single | Self::Double | Self::UserDefined(_))
    }
    /// Tests if the type is any numeric type, including integers and floats
    /// (basically tests for non-aggregate types).
    #[inline]
    pub fn is_numeric(&self) -> bool {
        !matches!(self, Self::UserDefined(_))
    }
    /// Tests if the type is a floating-point type.
    #[inline]
    pub fn is_floating(&self) -> bool {
        matches!(self, Self::Single | Self::Double)
    }
    /// Tests if the type could represent a pointer value.
    #[inline]
    pub fn is_pointer(&self) -> bool {
        matches!(self, Self::Long | Self::UserDefined(_))
    }
}
impl<W: Write> QbeCodegen<W> for QbeType {
    #[inline]
    fn gen(&self, d: &mut W) -> fmt::Result {
        match self {
            Self::Word => d.write_str("w"),
            Self::Long => d.write_str("l"),
            Self::Single => d.write_str("s"),
            Self::Double => d.write_str("d"),
            Self::Byte => d.write_str("ub"),
            Self::Half => d.write_str("uh"),
            Self::SignedByte => d.write_str("sb"),
            Self::SignedHalf => d.write_str("sh"),
            Self::UserDefined(id) => write!(d, ":_{}", id),
        }
    }
}

/// Type for compile-time data constants. These can be assigned to variables
/// that can later be used as QBE values via the `global` family of functions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum QbeData<'a> {
    /// Constant strings in the data.
    String(&'a str),
    /// Anonymous global variables (which are represented by numeric ids).
    Global(u32),
    /// Pointer offsets into anonymous global variables.
    OffsetGlobal(u32, u64),
    /// Constant integer values.
    Constant(QbeType, u64),
    /// Named global variables.
    Named(&'a str),
    /// Pointers offsets into named global variables.
    OffsetNamed(&'a str, u64),
}
impl<W: Write> QbeCodegen<W> for QbeData<'_> {
    #[inline]
    fn gen(&self, d: &mut W) -> fmt::Result {
        match self {
            Self::String(s) => write!(d, "b \"{}\"", s.escape_default()),
            Self::Global(id) => write!(d, "$_{}", id),
            Self::OffsetGlobal(id, offset) => write!(d, "$_{}+{}", id, offset),
            Self::Constant(typ, val) => write!(d, "{} {}", typ.basic_name(), val),
            Self::Named(name) => write!(d, "${}", name),
            Self::OffsetNamed(name, offset) => write!(d, "{}+{}", name, offset),
        }
    }
}
impl From<u8> for QbeData<'_> {
    #[inline]
    fn from(item: u8) -> Self {
        Self::Constant(QbeType::Byte, item as u64)
    }
}
impl From<u16> for QbeData<'_> {
    #[inline]
    fn from(item: u16) -> Self {
        Self::Constant(QbeType::Half, item as u64)
    }
}
impl From<u32> for QbeData<'_> {
    #[inline]
    fn from(item: u32) -> Self {
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<u64> for QbeData<'_> {
    #[inline]
    fn from(item: u64) -> Self {
        Self::Constant(QbeType::Long, item)
    }
}
impl From<i8> for QbeData<'_> {
    #[inline]
    fn from(item: i8) -> Self {
        Self::Constant(QbeType::Byte, item as u64)
    }
}
impl From<i16> for QbeData<'_> {
    #[inline]
    fn from(item: i16) -> Self {
        Self::Constant(QbeType::Half, item as u64)
    }
}
impl From<i32> for QbeData<'_> {
    #[inline]
    fn from(item: i32) -> Self {
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<i64> for QbeData<'_> {
    #[inline]
    fn from(item: i64) -> Self {
        Self::Constant(QbeType::Long, item as u64)
    }
}
impl From<f32> for QbeData<'_> {
    #[inline]
    fn from(item: f32) -> Self {
        Self::Constant(QbeType::Word, item.to_bits() as u64)
    }
}
impl From<f64> for QbeData<'_> {
    #[inline]
    fn from(item: f64) -> Self {
        Self::Constant(QbeType::Long, item.to_bits())
    }
}
impl<'a> From<&'a str> for QbeData<'a> {
    #[inline]
    fn from(item: &'a str) -> Self {
        Self::String(item)
    }
}
impl<'a> From<QbeValue<'a>> for QbeData<'a> {
    #[inline]
    fn from(item: QbeValue<'a>) -> Self {
        match item {
            QbeValue::Global(id) => Self::Global(id),
            QbeValue::Named(name) => Self::Named(name),
            _ => panic!("cannot use one of those"),
        }
    }
}
impl From<&QbeForwardDecl> for QbeData<'_> {
    #[inline]
    fn from(item: &QbeForwardDecl) -> Self {
        Self::Global(item.0)
    }
}

/// Labels that may be used as jump targets. Note that it is a logical error
/// (though not a compile- or runtime-error) to use a `QbeLabel` with a
/// [`crate::QbeContext`] or [`crate::QbeFunctionBuilder`] other than the one
/// that created it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct QbeLabel(pub(crate) u32);
impl<W: Write> QbeCodegen<W> for QbeLabel {
    #[inline]
    fn gen(&self, d: &mut W) -> fmt::Result {
        write!(d, "@_{}", self.0)
    }
}

/// The type of run-time QBE values, such as those created by operations or
/// external symbols used by the program. Note that it is a logical error
/// (though not a compile- or runtime-error) to use a `QbeValue` with a
/// [`crate::QbeContext`] or [`crate::QbeFunctionBuilder`] other than the one
/// that created it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum QbeValue<'a> {
    Global(u32),
    Temporary(QbeType, u32),
    Constant(QbeType, u64),
    Named(&'a str),
    ThreadLocalNamed(&'a str),
}
impl<'a> QbeValue<'a> {
    /// Returns the type of the value.
    #[inline]
    pub fn type_of(&self) -> QbeType {
        match self {
            Self::Global(_) => QbeType::Long,
            Self::Temporary(typ, _) => *typ,
            Self::Constant(typ, _) => *typ,
            Self::Named(_) => QbeType::Long,
            Self::ThreadLocalNamed(_) => QbeType::Long,
        }
    }

    /// Returns a common type, if able, between this and `with`. This is the
    /// type the values will be cast to if binary operations are used between
    /// these two values.
    pub fn common_type(&self, with: &'a QbeValue<'a>) -> Result<QbeType> {
        let typ = match self {
            Self::Global(_) => QbeType::Long,
            Self::Temporary(typ, _) => *typ,
            Self::Constant(_, _) => return Ok(with.type_of()),
            Self::Named(_) => QbeType::Long,
            Self::ThreadLocalNamed(_) => QbeType::Long,
        }
        .promote();
        let other = match with {
            Self::Global(_) => QbeType::Long,
            Self::Temporary(typ, _) => *typ,
            Self::Constant(_, _) => return Ok(typ),
            Self::Named(_) => QbeType::Long,
            Self::ThreadLocalNamed(_) => QbeType::Long,
        }
        .promote();
        if typ == other {
            return Ok(typ);
        }
        match (typ, other) {
            (QbeType::Word, QbeType::Long) => Ok(QbeType::Word),
            (QbeType::Long, QbeType::Word) => Ok(QbeType::Word),
            _ => Err(QbeError::CannotInferType),
        }
    }
}
impl<W: Write> QbeCodegen<W> for QbeValue<'_> {
    #[inline]
    fn gen(&self, d: &mut W) -> fmt::Result {
        match self {
            Self::Global(id) => write!(d, "$_{}", id),
            Self::Temporary(_, id) => write!(d, "%_{}", id),
            Self::Constant(_, val) => write!(d, "{}", val),
            Self::Named(name) => write!(d, "${}", name),
            Self::ThreadLocalNamed(name) => write!(d, "thread ${}", name),
        }
    }
}
impl From<u8> for QbeValue<'_> {
    #[inline]
    fn from(item: u8) -> Self {
        // automatic promotion from byte to word
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<u16> for QbeValue<'_> {
    #[inline]
    fn from(item: u16) -> Self {
        // automatic promotion from half to word
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<u32> for QbeValue<'_> {
    #[inline]
    fn from(item: u32) -> Self {
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<u64> for QbeValue<'_> {
    #[inline]
    fn from(item: u64) -> Self {
        Self::Constant(QbeType::Long, item)
    }
}
impl From<i8> for QbeValue<'_> {
    #[inline]
    fn from(item: i8) -> Self {
        // automatic promotion from byte to word
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<i16> for QbeValue<'_> {
    #[inline]
    fn from(item: i16) -> Self {
        // automatic promotion from half to word
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<i32> for QbeValue<'_> {
    #[inline]
    fn from(item: i32) -> Self {
        Self::Constant(QbeType::Word, item as u64)
    }
}
impl From<i64> for QbeValue<'_> {
    #[inline]
    fn from(item: i64) -> Self {
        Self::Constant(QbeType::Long, item as u64)
    }
}
impl From<f32> for QbeValue<'_> {
    #[inline]
    fn from(item: f32) -> Self {
        Self::Constant(QbeType::Single, item.to_bits() as u64)
    }
}
impl From<f64> for QbeValue<'_> {
    #[inline]
    fn from(item: f64) -> Self {
        Self::Constant(QbeType::Double, item.to_bits())
    }
}
impl<'a> From<&'a str> for QbeValue<'a> {
    fn from(item: &'a str) -> Self {
        Self::Named(item)
    }
}
impl<'a, Out: QbeFunctionOutput<'a>> From<&QbeFunction<'a, Out>> for QbeValue<'a> {
    #[inline]
    fn from(item: &QbeFunction<'a, Out>) -> Self {
        match item.inner {
            QbeFunctionInner::Global(id) => Self::Global(id),
            QbeFunctionInner::Named(name) => Self::Named(name),
        }
    }
}
impl<'a, Out: QbeFunctionOutput<'a>> From<&QbeVariadicFunction<'a, Out>> for QbeValue<'a> {
    #[inline]
    fn from(item: &QbeVariadicFunction<'a, Out>) -> Self {
        match item.inner {
            QbeFunctionInner::Global(id) => Self::Global(id),
            QbeFunctionInner::Named(name) => Self::Named(name),
        }
    }
}

// for ease of design, a forward declaration cannot use `export_as`
// if this is desired, use a global named symbol instead
// doesn't derive `Copy` or `Clone` to disallow redeclaring a symbol
/// A forward declaration of a label. Can borrow to `QbeValue`, but is moved
/// when creating a value from it to help prevent issues, though without
/// [must move values](https://smallcultfollowing.com/babysteps/blog/2023/03/16/must-move-types/)
/// in Rust, there's nothing preventing someone from creating a forward
/// declaration and simply never declaring it.
#[derive(Debug, PartialEq, Eq)]
pub struct QbeForwardDecl(pub(crate) u32);
impl From<&QbeForwardDecl> for QbeValue<'_> {
    #[inline]
    fn from(item: &QbeForwardDecl) -> Self {
        Self::Global(item.0)
    }
}

/// A forward declaration of a label. Can borrow to `QbeLabel`, but is moved
/// when creating a label from it to help prevent issues, though without
/// [must move values](https://smallcultfollowing.com/babysteps/blog/2023/03/16/must-move-types/)
/// in Rust, there's nothing preventing someone from creating a forward
/// declaration and simply never declaring it.
#[derive(Debug, PartialEq, Eq)]
pub struct QbeForwardLabel(pub(crate) u32);
impl From<&QbeForwardLabel> for QbeLabel {
    #[inline]
    fn from(item: &QbeForwardLabel) -> Self {
        QbeLabel(item.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum QbeFunctionInner<'a> {
    Global(u32),
    Named(&'a str),
}
impl<'a, W: Write> QbeCodegen<W> for QbeFunctionInner<'a> {
    #[inline]
    fn gen(&self, dest: &mut W) -> fmt::Result {
        match self {
            Self::Global(id) => write!(dest, "$_{}", id),
            Self::Named(name) => write!(dest, "${}", name),
        }
    }
}

/// Type of normal (non-variadic) QBE functions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct QbeFunction<'a, Out: QbeFunctionOutput<'a>> {
    pub(crate) inner: QbeFunctionInner<'a>,
    pub(crate) ud: Out::UserData,
}
impl<'a, Out: QbeFunctionOutput<'a>> QbeFunctionCall<'a> for QbeFunction<'a, Out> {
    type Output = Out;
    fn call_on<CallerOut, I, A, const V: bool>(
        &self,
        caller: &mut QbeFunctionBuilder<'a, CallerOut, V>,
        args: I,
    ) -> Result<Out>
    where
        CallerOut: QbeFunctionOutput<'a>,
        I: IntoIterator<Item = A>,
        A: Into<QbeValue<'a>>,
    {
        caller.compiled.write_char('\t')?;
        Out::prep_call(caller, &self.ud)?;
        caller.compiled.write_str("call ")?;
        self.inner.gen(&mut caller.compiled)?;
        caller.compiled.write_char('(')?;
        for arg in args {
            let arg = arg.into();
            arg.type_of().gen(&mut caller.compiled)?;
            caller.compiled.write_char(' ')?;
            arg.gen(&mut caller.compiled)?;
            caller.compiled.write_str(", ")?;
        }
        caller.compiled.write_str(")\n")?;
        Out::finish_call(caller, &self.ud)
    }
}

/// Type of variadic QBE functions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct QbeVariadicFunction<'a, Out: QbeFunctionOutput<'a>> {
    pub(crate) inner: QbeFunctionInner<'a>,
    pub(crate) ud: Out::UserData,
}
impl<'a, Out: QbeFunctionOutput<'a>> QbeVariadicFunctionCall<'a> for QbeVariadicFunction<'a, Out> {
    type Output = Out;
    fn call_va_on<CallerOut, I, A, const V: bool>(
        &self,
        caller: &mut QbeFunctionBuilder<'a, CallerOut, V>,
        args: I,
        va_args: I,
    ) -> Result<Out>
    where
        CallerOut: QbeFunctionOutput<'a>,
        I: IntoIterator<Item = A>,
        A: Into<QbeValue<'a>>,
    {
        caller.compiled.write_char('\t')?;
        Out::prep_call(caller, &self.ud)?;
        caller.compiled.write_str("call ")?;
        self.inner.gen(&mut caller.compiled)?;
        caller.compiled.write_char('(')?;
        for arg in args {
            let arg = arg.into();
            arg.type_of().gen(&mut caller.compiled)?;
            caller.compiled.write_char(' ')?;
            arg.gen(&mut caller.compiled)?;
            caller.compiled.write_str(", ")?;
        }
        caller.compiled.write_str("..., ")?;
        for arg in va_args {
            let arg = arg.into();
            arg.type_of().gen(&mut caller.compiled)?;
            caller.compiled.write_char(' ')?;
            arg.gen(&mut caller.compiled)?;
            caller.compiled.write_str(", ")?;
        }
        caller.compiled.write_str(")\n")?;
        Out::finish_call(caller, &self.ud)
    }
}

/// A trait used for types which are valid function returns from QBE functions.
/// Mainly an implementation detail used to let the Rust type system determine
/// some mismatches in a function's return value; the function used to generate
/// QBE functions must return a value of the same type everywhere, though the
/// Rust compiler has no information into the QBE type of a QBE value.
pub trait QbeFunctionOutput<'a>: Sized + Copy {
    type UserData: PartialEq + Copy;
    fn prep_call<CallerOut, const V: bool>(
        caller: &mut QbeFunctionBuilder<'a, CallerOut, V>,
        ud: &Self::UserData,
    ) -> Result<()>
    where
        CallerOut: QbeFunctionOutput<'a>;
    fn finish_call<CallerOut, const V: bool>(
        caller: &mut QbeFunctionBuilder<'a, CallerOut, V>,
        ud: &Self::UserData,
    ) -> Result<Self>
    where
        CallerOut: QbeFunctionOutput<'a>;
    fn prep_func(&self, compiled: &mut String) -> Result<()>;
    fn func_return<const V: bool>(&self, func: &mut QbeFunctionBuilder<'a, Self, V>) -> Result<()>;
    fn get_ud(&self) -> Self::UserData;
}
impl<'a> QbeFunctionOutput<'a> for () {
    type UserData = ();
    #[inline]
    fn prep_call<CallerOut, const V: bool>(
        _caller: &mut QbeFunctionBuilder<'a, CallerOut, V>,
        _: &(),
    ) -> Result<()>
    where
        CallerOut: QbeFunctionOutput<'a>,
    {
        Ok(())
    }
    #[inline]
    fn finish_call<CallerOut, const V: bool>(
        _caller: &mut QbeFunctionBuilder<'a, CallerOut, V>,
        _: &(),
    ) -> Result<Self>
    where
        CallerOut: QbeFunctionOutput<'a>,
    {
        Ok(())
    }
    #[inline]
    fn prep_func(&self, compiled: &mut String) -> Result<()> {
        compiled.write_str("function ")?;
        Ok(())
    }
    #[inline]
    fn func_return<const V: bool>(&self, func: &mut QbeFunctionBuilder<'a, Self, V>) -> Result<()> {
        func.compiled.write_str("\tret\n")?;
        Ok(())
    }
    #[inline]
    fn get_ud(&self) {}
}
impl<'a> QbeFunctionOutput<'a> for QbeValue<'a> {
    type UserData = QbeType;
    fn prep_call<CallerOut, const V: bool>(
        caller: &mut QbeFunctionBuilder<'a, CallerOut, V>,
        typ: &QbeType,
    ) -> Result<()>
    where
        CallerOut: QbeFunctionOutput<'a>,
    {
        let id = caller.local_counter;
        write!(&mut caller.compiled, "%_{} =", id)?;
        typ.gen(&mut caller.compiled)?;
        caller.compiled.write_char(' ')?;
        Ok(())
    }
    fn finish_call<CallerOut, const V: bool>(
        caller: &mut QbeFunctionBuilder<'a, CallerOut, V>,
        typ: &QbeType,
    ) -> Result<Self>
    where
        CallerOut: QbeFunctionOutput<'a>,
    {
        let id = caller.local_counter;
        caller.local_counter += 1;
        Ok(QbeValue::Temporary(*typ, id))
    }
    fn prep_func(&self, compiled: &mut String) -> Result<()> {
        compiled.write_str("function ")?;
        let typ = self.type_of().promote();
        typ.gen(compiled)?;
        compiled.write_char(' ')?;
        Ok(())
    }
    fn func_return<const V: bool>(&self, func: &mut QbeFunctionBuilder<'a, Self, V>) -> Result<()> {
        func.compiled.write_str("\tret ")?;
        self.gen(&mut func.compiled)?;
        func.compiled.write_char('\n')?;
        Ok(())
    }
    #[inline]
    fn get_ud(&self) -> QbeType {
        self.type_of().promote()
    }
}

/// External functions declared from `extern_func`; similar to external symbols,
/// but implements the `QbeFunctionCall` and `QbeVariadicFunctionCall` traits to
/// be callable as a function.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct QbeExternFunction<'a, Out: QbeFunctionOutput<'a>> {
    pub(crate) name: &'a str,
    pub(crate) ud: Out::UserData,
}
impl<'a, Out: QbeFunctionOutput<'a>> QbeFunctionCall<'a> for QbeExternFunction<'a, Out> {
    type Output = Out;
    fn call_on<CallerOut, I, A, const V: bool>(
        &self,
        caller: &mut QbeFunctionBuilder<'a, CallerOut, V>,
        args: I,
    ) -> Result<Out>
    where
        CallerOut: QbeFunctionOutput<'a>,
        I: IntoIterator<Item = A>,
        A: Into<QbeValue<'a>>,
    {
        caller.compiled.write_char('\t')?;
        Out::prep_call(caller, &self.ud)?;
        caller.compiled.write_str("call $")?;
        caller.compiled.write_str(self.name)?;
        caller.compiled.write_char('(')?;
        for arg in args {
            let arg = arg.into();
            arg.type_of().gen(&mut caller.compiled)?;
            caller.compiled.write_char(' ')?;
            arg.gen(&mut caller.compiled)?;
            caller.compiled.write_str(", ")?;
        }
        caller.compiled.write_str(")\n")?;
        Out::finish_call(caller, &self.ud)
    }
}
impl<'a, Out: QbeFunctionOutput<'a>> QbeVariadicFunctionCall<'a> for QbeExternFunction<'a, Out> {
    type Output = Out;
    fn call_va_on<CallerOut, I, A, const V: bool>(
        &self,
        caller: &mut QbeFunctionBuilder<'a, CallerOut, V>,
        args: I,
        va_args: I,
    ) -> Result<Out>
    where
        CallerOut: QbeFunctionOutput<'a>,
        I: IntoIterator<Item = A>,
        A: Into<QbeValue<'a>>,
    {
        caller.compiled.write_char('\t')?;
        Out::prep_call(caller, &self.ud)?;
        caller.compiled.write_str("call $")?;
        caller.compiled.write_str(self.name)?;
        caller.compiled.write_char('(')?;
        for arg in args {
            let arg = arg.into();
            arg.type_of().gen(&mut caller.compiled)?;
            caller.compiled.write_char(' ')?;
            arg.gen(&mut caller.compiled)?;
            caller.compiled.write_str(", ")?;
        }
        caller.compiled.write_str("..., ")?;
        for arg in va_args {
            let arg = arg.into();
            arg.type_of().gen(&mut caller.compiled)?;
            caller.compiled.write_char(' ')?;
            arg.gen(&mut caller.compiled)?;
            caller.compiled.write_str(", ")?;
        }
        caller.compiled.write_str(")\n")?;
        Out::finish_call(caller, &self.ud)
    }
}
