use crate::func::{
    QbeFunctionBuilder, QbeFunctionCall, QbeVariadicFunctionCall, NON_VARIADIC_FUNC, VARIADIC_FUNC,
};
use crate::value::{
    QbeCodegen, QbeData, QbeForwardDecl, QbeFunction, QbeFunctionInner, QbeFunctionOutput, QbeType,
    QbeValue, QbeVariadicFunction,
};
#[cfg(feature = "qbe-compile")]
use crate::{
    qbe_wrapper::{write_assembly_to_file, write_assembly_to_string, CFile},
    QbeTarget,
};
use crate::{QbeError, Result};
use std::cell::UnsafeCell;
use std::fmt::Write;
use std::pin::Pin;

/// Options for a global QBE declaration (variable or function), allowing you to
/// specify things like exported name, alignment, section, and thread-local.
#[derive(Copy, Clone, Debug, Default)]
pub struct QbeDecl<'a> {
    /// Inform the linker what section to place the symbol in.
    pub section: Option<&'a str>,
    /// Add additional flags to the section definition. The meaning of these is
    /// platform dependent.
    pub section_flags: Option<&'a str>,
    /// Place the object in thread-local storage.
    pub thread_local: bool,
    /// Align the object to this many bytes, instead of using the default
    /// alignment for the symbol's type. Note that this option isn't valid for
    /// function declaration, only values.
    pub align_to: Option<u64>,
    /// Export the symbol under this name. By default, symbols aren't exported,
    /// and the names are unspecified and generated by the library; this option
    /// is needed to have defined names for symbols.
    pub export_as: Option<&'a str>,
}

impl<'a> QbeDecl<'a> {
    pub fn section(self, sec: &'a str) -> Self {
        QbeDecl {
            section: Some(sec),
            ..self
        }
    }
    pub fn section_flags(self, flags: &'a str) -> Self {
        QbeDecl {
            section_flags: Some(flags),
            ..self
        }
    }
    pub fn thread_local(self, thread_local: bool) -> Self {
        QbeDecl {
            thread_local,
            ..self
        }
    }
    pub fn align_to(self, alignment: u64) -> Self {
        QbeDecl {
            align_to: Some(alignment),
            ..self
        }
    }
    pub fn export_as(self, name: &'a str) -> Self {
        QbeDecl {
            export_as: Some(name),
            ..self
        }
    }
}

#[derive(Default, Debug)]
struct QbeContextInner {
    global_counter: u32,
    type_counter: u32,
    compiled: String,
    names: Vec<Pin<Box<str>>>,
}

/// The context object holding the buffer QBE IR is written to. A `QbeContext`
/// is needed to emit any QBE code. Note that these objects are not thread-safe.
// `UnsafeCell` to avoid complications where `QbeValue`s and similarly created
// objects, because they can't outlive the context (due to `&str` pointers to
// `names`), keep mutable references; lifetimes without borrowing (just some
// mechanism where a function taking `&mut self` can return something that can't
// outlive `self` without needing to hold the mutable reference to `self`)
// would, as best I can tell, be required to solve this little problem.
// Because `UnsafeCell` isn't `Sync`, I think the safety for this holds; every
// function takes `&self` and essentially just uses it as `&mut self`, but if no
// two functions can both be operating on `self` at the same time, it may not be
// an issue.
#[derive(Default, Debug)]
pub struct QbeContext(UnsafeCell<QbeContextInner>);

impl QbeContext {
    /// Create a new `QbeContext`. Equivalent to [`QbeContext::default()`].
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    // global variable definitions
    /// Create a global variable with initial value `val`. This method takes no
    /// declaration options, so it is left at the default settings and not
    /// exported publicly.
    pub fn global<'a, T: Into<QbeData<'a>>>(&self, val: T) -> Result<QbeValue> {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let data = val.into();
        let id = this.global_counter;
        write!(&mut this.compiled, "data $_{} = {{ ", id)?;
        data.gen(&mut this.compiled)?;
        this.compiled.write_str(" }\n")?;
        this.global_counter += 1;
        Ok(QbeValue::Global(id))
    }
    /// Initialize a forward-declared global variable `at` with value `val`.
    /// Because this consumes `at`, forward-declared variables can only be
    /// initialized once. Similarly to [`global`][Self::global], this takes no
    /// options, so the variable isn't exported.
    pub fn global_at<'a, T: Into<QbeData<'a>>>(
        &self,
        at: QbeForwardDecl,
        val: T,
    ) -> Result<QbeValue> {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let data = val.into();
        let id = at.0;
        writeln!(&mut this.compiled, "data $_{} = {{ ", id)?;
        data.gen(&mut this.compiled)?;
        this.compiled.write_str(" }\n")?;
        Ok(QbeValue::Global(id))
    }
    /// Create a global variable with initial value `val`, using `opts` to
    /// configure how the variable is created.
    pub fn global_ext<'a, T: Into<QbeData<'a>>>(&self, val: T, opts: &QbeDecl) -> Result<QbeValue> {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let data = val.into();
        if opts.thread_local {
            writeln!(&mut this.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut this.compiled, "section {} {}", sec, flags)?;
        }
        if let Some(name) = opts.export_as {
            writeln!(&mut this.compiled, "export")?;
            let s = Box::into_pin(Box::<str>::from(name));
            this.names.push(s);
            let name = &this.names[this.names.len() - 1];
            if let Some(align) = opts.align_to {
                writeln!(&mut this.compiled, "data ${} = align {} {{ ", name, align)?;
            } else {
                writeln!(&mut this.compiled, "data ${} = {{ ", name)?;
            }
            data.gen(&mut this.compiled)?;
            this.compiled.write_str(" }\n")?;
            Ok(QbeValue::Named(name))
        } else {
            let id = this.global_counter;
            if let Some(align) = opts.align_to {
                writeln!(&mut this.compiled, "data $_{} = align {} {{ ", id, align)?;
            } else {
                writeln!(&mut this.compiled, "data $_{} = {{ ", id)?;
            }
            data.gen(&mut this.compiled)?;
            this.compiled.write_str(" }\n")?;
            this.global_counter += 1;
            Ok(QbeValue::Global(id))
        }
    }
    /// Initialize a forward-declared global variable `at` with value `val`,
    /// using `opts` to configure how the variable is created. Because this
    /// consumes `at`, forward-declared variables can only be initialized once.
    ///
    /// Note that, due to current implementation, specifying
    /// [`export_as`][QbeDecl::export_as] with a forward-declared variable is an
    /// an error. If this is desired, use [`global_symbol`][Self::global_symbol]
    /// with the exported name before initializing the variable, and initialize
    /// the variable using the normal [`global_ext`][Self::global_ext] function.
    pub fn global_ext_at<'a, T: Into<QbeData<'a>>>(
        &self,
        at: QbeForwardDecl,
        val: T,
        opts: &QbeDecl,
    ) -> Result<QbeValue> {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let data = val.into();
        let id = at.0;
        if opts.export_as.is_some() {
            return Err(QbeError::ForwardDeclareName);
        }
        if opts.thread_local {
            writeln!(&mut this.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut this.compiled, "section {} {}", sec, flags)?;
        }
        if let Some(align) = opts.align_to {
            writeln!(&mut this.compiled, "data $_{} = align {} {{ ", id, align)?;
        } else {
            writeln!(&mut this.compiled, "data $_{} = {{ ", id)?;
        }
        data.gen(&mut this.compiled)?;
        this.compiled.write_str(" }\n")?;
        Ok(QbeValue::Global(id))
    }
    pub fn global_zeroed(&self, size: u64) -> Result<QbeValue> {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let id = this.global_counter;
        writeln!(&mut this.compiled, "data $_{} = {{ z {} }}", id, size)?;
        this.global_counter += 1;
        Ok(QbeValue::Global(id))
    }
    pub fn global_zeroed_at(&self, at: QbeForwardDecl, size: u64) -> Result<QbeValue> {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let id = at.0;
        writeln!(&mut this.compiled, "data $_{} = {{ z {} }}", id, size)?;
        Ok(QbeValue::Global(id))
    }
    pub fn global_zeroed_ext(&self, size: u64, opts: &QbeDecl) -> Result<QbeValue> {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        if opts.thread_local {
            writeln!(&mut this.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut this.compiled, "section {} {}", sec, flags)?;
        }
        if let Some(name) = opts.export_as {
            writeln!(&mut this.compiled, "export")?;
            let s = Box::into_pin(Box::<str>::from(name));
            this.names.push(s);
            let name = &this.names[this.names.len() - 1];
            if let Some(align) = opts.align_to {
                writeln!(
                    &mut this.compiled,
                    "data ${} = align {} {{ z {} }}",
                    name, align, size
                )?;
            } else {
                writeln!(&mut this.compiled, "data ${} = {{ z {} }}", name, size)?;
            }
            Ok(QbeValue::Named(name))
        } else {
            let id = this.global_counter;
            if let Some(align) = opts.align_to {
                writeln!(
                    &mut this.compiled,
                    "data $_{} = align {} {{ z {} }}",
                    id, align, size
                )?;
            } else {
                writeln!(&mut this.compiled, "data $_{} = {{ z {} }}", id, size)?;
            }
            this.global_counter += 1;
            Ok(QbeValue::Global(id))
        }
    }
    pub fn global_zeroed_ext_at(
        &self,
        at: QbeForwardDecl,
        size: u64,
        opts: &QbeDecl,
    ) -> Result<QbeValue> {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let id = at.0;
        if opts.export_as.is_some() {
            return Err(QbeError::ForwardDeclareName);
        }
        if opts.thread_local {
            writeln!(&mut this.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut this.compiled, "section {} {}", sec, flags)?;
        }
        if let Some(align) = opts.align_to {
            writeln!(
                &mut this.compiled,
                "data $_{} = align {} {{ z {} }}",
                id, align, size
            )?;
        } else {
            writeln!(&mut this.compiled, "data $_{} = {{ z {} }}", id, size)?;
        }
        Ok(QbeValue::Global(id))
    }
    pub fn global_symbol(&self, sym: &str) -> QbeValue {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let s = Box::into_pin(Box::<str>::from(sym));
        this.names.push(s);
        QbeValue::Named(&this.names[this.names.len() - 1])
    }

    // type definitions
    pub fn opaque_type(&self, align: u64, size: u64) -> Result<QbeType> {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let id = this.type_counter;
        writeln!(
            &mut this.compiled,
            "type :_{} = align {} {{ {} }}",
            id, align, size
        )?;
        this.type_counter += 1;
        Ok(QbeType::UserDefined(id))
    }
    pub fn struct_type(&self, members: &[QbeType]) -> Result<QbeType> {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let id = this.type_counter;
        write!(&mut this.compiled, "type :_{} = {{", id)?;
        for memb in members {
            if !memb.is_numeric() {
                return Err(QbeError::NotBasic);
            }
            write!(&mut this.compiled, "{}, ", memb.basic_name())?;
        }
        this.compiled.write_str("}\n")?;
        this.type_counter += 1;
        Ok(QbeType::UserDefined(id))
    }
    pub fn struct_type_align(&self, members: &[QbeType], align: u64) -> Result<QbeType> {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let id = this.type_counter;
        write!(&mut this.compiled, "type :_{} = align {} {{", id, align)?;
        for memb in members {
            if !memb.is_numeric() {
                return Err(QbeError::NotBasic);
            }
            write!(&mut this.compiled, "{}, ", memb.basic_name())?;
        }
        this.compiled.write_str("}\n")?;
        this.type_counter += 1;
        Ok(QbeType::UserDefined(id))
    }

    // function definition
    pub(crate) fn make_function<'a, const VARIADIC: bool, Out, F>(
        &'a self,
        name: impl QbeCodegen<String>,
        params: &'a [QbeType],
        builder: F,
    ) -> Result<Out::UserData>
    where
        Out: QbeFunctionOutput<'a>,
        F: FnOnce(&mut QbeFunctionBuilder<'a, Out, VARIADIC>) -> Result<Out>,
    {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let mut f = QbeFunctionBuilder::<Out, VARIADIC>::new(params, &mut this.names);

        let out = f.build(builder)?;
        out.prep_func(&mut this.compiled)?;

        name.gen(&mut this.compiled)?;
        this.compiled.write_char('(')?;

        for (count, memb) in params.iter().enumerate() {
            // user defined types are passed as pointers
            memb.pointer_ud().gen(&mut this.compiled)?;
            write!(&mut this.compiled, " %_{}, ", count)?;
        }

        this.compiled.write_str(") {\n")?;
        this.compiled.write_str(&f.compiled)?;
        this.compiled.write_str("}\n")?;
        Ok(out.get_ud())
    }
    pub fn function<'a, Out, F>(
        &'a self,
        params: &'a [QbeType],
        builder: F,
    ) -> Result<impl QbeFunctionCall<'a> + Copy>
    where
        Out: QbeFunctionOutput<'a>,
        F: FnOnce(&mut QbeFunctionBuilder<'a, Out, NON_VARIADIC_FUNC>) -> Result<Out>,
    {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let name = QbeFunctionInner::Global(this.global_counter);
        let ud = self.make_function(name, params, builder)?;
        this.global_counter += 1;
        Ok(QbeFunction::<Out> { inner: name, ud })
    }
    pub fn function_at<'a, Out, F>(
        &'a self,
        at: QbeForwardDecl,
        params: &'a [QbeType],
        builder: F,
    ) -> Result<impl QbeFunctionCall<'a> + Copy>
    where
        Out: QbeFunctionOutput<'a>,
        F: FnOnce(&mut QbeFunctionBuilder<'a, Out, NON_VARIADIC_FUNC>) -> Result<Out>,
    {
        let name = QbeFunctionInner::Global(at.0);
        let ud = self.make_function(name, params, builder)?;
        Ok(QbeFunction::<Out> { inner: name, ud })
    }
    pub fn function_ext<'a, Out, F>(
        &'a self,
        params: &'a [QbeType],
        opts: &QbeDecl,
        builder: F,
    ) -> Result<impl QbeFunctionCall<'a> + Copy>
    where
        Out: QbeFunctionOutput<'a>,
        F: FnOnce(&mut QbeFunctionBuilder<'a, Out, NON_VARIADIC_FUNC>) -> Result<Out>,
    {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let out_name = if let Some(name) = opts.export_as {
            let s = Box::into_pin(Box::<str>::from(name));
            this.names.push(s);
            QbeFunctionInner::Named(&this.names[this.names.len() - 1])
        } else {
            let id = this.global_counter;
            QbeFunctionInner::Global(id)
        };

        if opts.thread_local {
            writeln!(&mut this.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut this.compiled, "section {} {}", sec, flags)?;
        }
        if opts.export_as.is_some() {
            writeln!(&mut this.compiled, "export")?;
        }

        let ud = self.make_function(out_name, params, builder)?;
        if opts.export_as.is_none() {
            this.global_counter += 1;
        }
        Ok(QbeFunction::<Out> {
            inner: out_name,
            ud,
        })
    }
    pub fn function_ext_at<'a, Out, F>(
        &'a self,
        at: QbeForwardDecl,
        params: &'a [QbeType],
        opts: &QbeDecl,
        builder: F,
    ) -> Result<impl QbeFunctionCall<'a> + Copy>
    where
        Out: QbeFunctionOutput<'a>,
        F: FnOnce(&mut QbeFunctionBuilder<'a, Out, NON_VARIADIC_FUNC>) -> Result<Out>,
    {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        if opts.export_as.is_some() {
            return Err(QbeError::ForwardDeclareName);
        }

        if opts.thread_local {
            writeln!(&mut this.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut this.compiled, "section {} {}", sec, flags)?;
        }
        if opts.export_as.is_some() {
            writeln!(&mut this.compiled, "export")?;
        }

        let name = QbeFunctionInner::Global(at.0);
        let ud = self.make_function(name, params, builder)?;
        Ok(QbeFunction::<Out> { inner: name, ud })
    }

    pub fn va_function<'a, Out, F>(
        &'a self,
        params: &'a [QbeType],
        builder: F,
    ) -> Result<impl QbeVariadicFunctionCall<'a> + Copy>
    where
        Out: QbeFunctionOutput<'a>,
        F: FnOnce(&mut QbeFunctionBuilder<'a, Out, VARIADIC_FUNC>) -> Result<Out>,
    {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let name = QbeFunctionInner::Global(this.global_counter);
        let ud = self.make_function(name, params, builder)?;
        this.global_counter += 1;
        Ok(QbeVariadicFunction::<Out> { inner: name, ud })
    }
    pub fn va_function_at<'a, Out, F>(
        &'a self,
        at: QbeForwardDecl,
        params: &'a [QbeType],
        builder: F,
    ) -> Result<impl QbeVariadicFunctionCall<'a> + Copy>
    where
        Out: QbeFunctionOutput<'a>,
        F: FnOnce(&mut QbeFunctionBuilder<'a, Out, VARIADIC_FUNC>) -> Result<Out>,
    {
        let name = QbeFunctionInner::Global(at.0);
        let ud = self.make_function(name, params, builder)?;
        Ok(QbeVariadicFunction::<Out> { inner: name, ud })
    }
    pub fn va_function_ext<'a, Out, F>(
        &'a self,
        params: &'a [QbeType],
        opts: &QbeDecl,
        builder: F,
    ) -> Result<impl QbeVariadicFunctionCall<'a> + Copy>
    where
        Out: QbeFunctionOutput<'a>,
        F: FnOnce(&mut QbeFunctionBuilder<'a, Out, VARIADIC_FUNC>) -> Result<Out>,
    {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let out_name = if let Some(name) = opts.export_as {
            let s = Box::into_pin(Box::<str>::from(name));
            this.names.push(s);
            QbeFunctionInner::Named(&this.names[this.names.len() - 1])
        } else {
            let id = this.global_counter;
            QbeFunctionInner::Global(id)
        };

        if opts.thread_local {
            writeln!(&mut this.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut this.compiled, "section {} {}", sec, flags)?;
        }
        if opts.export_as.is_some() {
            writeln!(&mut this.compiled, "export")?;
        }

        let ud = self.make_function(out_name, params, builder)?;
        if opts.export_as.is_none() {
            this.global_counter += 1;
        }
        Ok(QbeVariadicFunction::<Out> {
            inner: out_name,
            ud,
        })
    }
    pub fn va_function_ext_at<'a, Out, F>(
        &'a self,
        at: QbeForwardDecl,
        params: &'a [QbeType],
        opts: &QbeDecl,
        builder: F,
    ) -> Result<impl QbeVariadicFunctionCall<'a> + Copy>
    where
        Out: QbeFunctionOutput<'a>,
        F: FnOnce(&mut QbeFunctionBuilder<'a, Out, VARIADIC_FUNC>) -> Result<Out>,
    {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        if opts.export_as.is_some() {
            return Err(QbeError::ForwardDeclareName);
        }

        if opts.thread_local {
            writeln!(&mut this.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut this.compiled, "section {} {}", sec, flags)?;
        }
        if opts.export_as.is_some() {
            writeln!(&mut this.compiled, "export")?;
        }

        let name = QbeFunctionInner::Global(at.0);
        let ud = self.make_function(name, params, builder)?;
        Ok(QbeVariadicFunction::<Out> { inner: name, ud })
    }

    #[inline]
    pub fn forward_declare(&self) -> QbeForwardDecl {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let id = this.global_counter;
        this.global_counter += 1;
        QbeForwardDecl(id)
    }

    #[inline]
    pub fn to_ir(self) -> String {
        self.0.into_inner().compiled
    }

    #[cfg(all(
        not(windows),
        any(
            target_arch = "x86_64",
            target_arch = "aarch64",
            target_arch = "riscv64gc"
        ),
        feature = "qbe-compile"
    ))]
    #[inline]
    pub fn write_assembly_to_file(self, file_name: &str) -> std::result::Result<(), errno::Errno> {
        let compiled = self.0.into_inner().compiled;
        let f = CFile::open(file_name, b"w\0")?;
        write_assembly_to_file(&compiled, QbeTarget::default(), &f)
    }
    #[cfg(feature = "qbe-compile")]
    #[inline]
    pub fn write_target_assembly_to_file(
        self,
        file_name: &str,
        target: QbeTarget,
    ) -> std::result::Result<(), errno::Errno> {
        let compiled = self.0.into_inner().compiled;
        let f = CFile::open(file_name, b"w\0")?;
        write_assembly_to_file(&compiled, target, &f)
    }
    #[cfg(all(
        not(windows),
        any(
            target_arch = "x86_64",
            target_arch = "aarch64",
            target_arch = "riscv64gc"
        ),
        feature = "qbe-compile"
    ))]
    #[inline]
    pub fn to_assembly(self) -> std::result::Result<String, errno::Errno> {
        let compiled = self.0.into_inner().compiled;
        write_assembly_to_string(&compiled, QbeTarget::default())
    }
    #[cfg(feature = "qbe-compile")]
    #[inline]
    pub fn to_target_assembly(
        self,
        target: QbeTarget,
    ) -> std::result::Result<String, errno::Errno> {
        let compiled = self.0.into_inner().compiled;
        write_assembly_to_string(&compiled, target)
    }
}
