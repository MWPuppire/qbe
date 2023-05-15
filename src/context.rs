use std::fmt::Write;
use std::pin::Pin;
use std::cell::UnsafeCell;

use crate::{Result, QbeError};
use crate::value::{
    QbeValue, QbeData, QbeType, QbeForwardDecl, QbeCodegen,
    QbeFunctionOutput, QbeFunction, QbeVariadicFunction, QbeFunctionInner
};
use crate::func::{
    QbeFunctionBuilder, QbeFunctionCall, QbeVariadicFunctionCall,
    VARIADIC_FUNC, NON_VARIADIC_FUNC
};
use crate::qbe_wrapper::{QbeTarget, CFile, write_assembly_to_string, write_assembly_to_file};

#[derive(Clone, Debug, Builder)]
pub struct QbeDecl<'a> {
    #[builder(setter(strip_option), default)]
    section: Option<&'a str>,
    #[builder(setter(strip_option), default)]
    section_flags: Option<&'a str>,
    #[builder(setter(strip_option), default)]
    thread_local: bool,
    #[builder(setter(strip_option), default)]
    align_to: Option<u64>,
    #[builder(setter(strip_option), default)]
    export_as: Option<&'a str>,
}

#[derive(Default, Debug)]
struct QbeContextInner {
    global_counter: u32,
    type_counter: u32,
    compiled: String,
    names: Vec<Pin<Box<str>>>,
}

#[derive(Default, Debug)]
pub struct QbeContext(UnsafeCell<QbeContextInner>);

impl QbeContext {
    pub fn new() -> Self {
        QbeContext::default()
    }

    // global variable definitions
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
    pub fn global_at<'a, T: Into<QbeData<'a>>>(&self, at: QbeForwardDecl, val: T) -> Result<QbeValue> {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let data = val.into();
        let id = at.0;
        writeln!(&mut this.compiled, "data $_{} = {{ ", id)?;
        data.gen(&mut this.compiled)?;
        this.compiled.write_str(" }\n")?;
        Ok(QbeValue::Global(id))
    }
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
    pub fn global_at_ext<'a, T: Into<QbeData<'a>>>(&self, at: QbeForwardDecl, val: T, opts: &QbeDecl) -> Result<QbeValue> {
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
                writeln!(&mut this.compiled, "data ${} = align {} {{ z {} }}", name, align, size)?;
            } else {
                writeln!(&mut this.compiled, "data ${} = {{ z {} }}", name, size)?;
            }
            Ok(QbeValue::Named(name))
        } else {
            let id = this.global_counter;
            if let Some(align) = opts.align_to {
                writeln!(&mut this.compiled, "data $_{} = align {} {{ z {} }}", id, align, size)?;
            } else {
                writeln!(&mut this.compiled, "data $_{} = {{ z {} }}", id, size)?;
            }
            this.global_counter += 1;
            Ok(QbeValue::Global(id))
        }
    }
    pub fn global_zeroed_at_ext(&self, at: QbeForwardDecl, size: u64, opts: &QbeDecl) -> Result<QbeValue> {
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
            writeln!(&mut this.compiled, "data $_{} = align {} {{ z {} }}", id, align, size)?;
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
        writeln!(&mut this.compiled, "type :_{} = align {} {{ {} }}", id, align, size)?;
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
    pub(crate) fn make_function<'a, const VARIADIC: bool, Out, F>(&'a self, name: impl QbeCodegen<String>, params: &'a [QbeType], builder: F) -> Result<Out::UserData>
    where Out: QbeFunctionOutput<'a>, F: FnOnce(&mut QbeFunctionBuilder<'a, Out, VARIADIC>) -> Result<Out> {
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
    pub fn function<'a, Out, F>(&'a self, params: &'a [QbeType], builder: F) -> Result<impl QbeFunctionCall<'a> + Copy>
    where Out: QbeFunctionOutput<'a>, F: FnOnce(&mut QbeFunctionBuilder<'a, Out, NON_VARIADIC_FUNC>) -> Result<Out> {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let name = QbeFunctionInner::Global(this.global_counter);
        let ud = self.make_function(name, params, builder)?;
        this.global_counter += 1;
        Ok(QbeFunction::<Out> {
            inner: name,
            ud,
        })
    }
    pub fn function_at<'a, Out, F>(&'a self, at: QbeForwardDecl, params: &'a [QbeType], builder: F) -> Result<impl QbeFunctionCall<'a> + Copy>
    where Out: QbeFunctionOutput<'a>, F: FnOnce(&mut QbeFunctionBuilder<'a, Out, NON_VARIADIC_FUNC>) -> Result<Out> {
        let name = QbeFunctionInner::Global(at.0);
        let ud = self.make_function(name, params, builder)?;
        Ok(QbeFunction::<Out> {
            inner: name,
            ud,
        })
    }
    pub fn function_ext<'a, Out, F>(&'a self, params: &'a [QbeType], opts: &QbeDecl, builder: F) -> Result<impl QbeFunctionCall<'a> + Copy>
    where Out: QbeFunctionOutput<'a>, F: FnOnce(&mut QbeFunctionBuilder<'a, Out, NON_VARIADIC_FUNC>) -> Result<Out> {
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
    pub fn function_ext_at<'a, Out, F>(&'a self, at: QbeForwardDecl, params: &'a [QbeType], opts: &QbeDecl, builder: F) -> Result<impl QbeFunctionCall<'a> + Copy>
    where Out: QbeFunctionOutput<'a>, F: FnOnce(&mut QbeFunctionBuilder<'a, Out, NON_VARIADIC_FUNC>) -> Result<Out> {
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
        Ok(QbeFunction::<Out> {
            inner: name,
            ud,
        })
    }

    pub fn va_function<'a, Out, F>(&'a self, params: &'a [QbeType], builder: F) -> Result<impl QbeVariadicFunctionCall<'a> + Copy>
    where Out: QbeFunctionOutput<'a>, F: FnOnce(&mut QbeFunctionBuilder<'a, Out, VARIADIC_FUNC>) -> Result<Out> {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let name = QbeFunctionInner::Global(this.global_counter);
        let ud = self.make_function(name, params, builder)?;
        this.global_counter += 1;
        Ok(QbeVariadicFunction::<Out> {
            inner: name,
            ud,
        })
    }
    pub fn va_function_at<'a, Out, F>(&'a self, at: QbeForwardDecl, params: &'a [QbeType], builder: F) -> Result<impl QbeVariadicFunctionCall<'a> + Copy>
    where Out: QbeFunctionOutput<'a>, F: FnOnce(&mut QbeFunctionBuilder<'a, Out, VARIADIC_FUNC>) -> Result<Out> {
        let name = QbeFunctionInner::Global(at.0);
        let ud = self.make_function(name, params, builder)?;
        Ok(QbeVariadicFunction::<Out> {
            inner: name,
            ud,
        })
    }
    pub fn va_function_ext<'a, Out, F>(&'a self, params: &'a [QbeType], opts: &QbeDecl, builder: F) -> Result<impl QbeVariadicFunctionCall<'a> + Copy>
    where Out: QbeFunctionOutput<'a>, F: FnOnce(&mut QbeFunctionBuilder<'a, Out, VARIADIC_FUNC>) -> Result<Out> {
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
    pub fn va_function_ext_at<'a, Out, F>(&'a self, at: QbeForwardDecl, params: &'a [QbeType], opts: &QbeDecl, builder: F) -> Result<impl QbeVariadicFunctionCall<'a> + Copy>
    where Out: QbeFunctionOutput<'a>, F: FnOnce(&mut QbeFunctionBuilder<'a, Out, VARIADIC_FUNC>) -> Result<Out> {
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
        Ok(QbeVariadicFunction::<Out> {
            inner: name,
            ud,
        })
    }

    pub fn forward_declare(&self) -> QbeForwardDecl {
        let this = unsafe { self.0.get().as_mut().unwrap_unchecked() };
        let id = this.global_counter;
        this.global_counter += 1;
        QbeForwardDecl(id)
    }

    pub fn compile(self) -> String {
        self.0.into_inner().compiled
    }

    pub fn write_assembly_to_file(self, file_name: &str) -> std::result::Result<(), errno::Errno> {
        let compiled = self.0.into_inner().compiled;
        let f = CFile::open(file_name, "w\0")?;
        write_assembly_to_file(&compiled, QbeTarget::default(), &f)?;
        Ok(())
    }
    pub fn to_assembly(self) -> std::result::Result<String, errno::Errno> {
        let compiled = self.0.into_inner().compiled;
        write_assembly_to_string(&compiled, QbeTarget::default())
    }
}
