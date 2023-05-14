use std::fmt::Write;
use std::pin::Pin;
use std::ops::Deref;

use crate::{Result, QbeError};
use crate::value::{
    QbeValue, QbeData, QbeType, QbeForwardDecl, QbeCodegen,
    QbeFunctionOutput, QbeFunction, QbeFunctionInner
};
use crate::func::{QbeFunctionBuilder, QbeFunctionCall, VARIADIC_FUNC, NON_VARIADIC_FUNC};
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

#[derive(Clone, Default, Debug)]
pub struct QbeContext {
    global_counter: u32,
    type_counter: u32,
    pub(crate) compiled: String,
    names: Vec<Pin<Box<str>>>,
}

impl QbeContext {
    pub fn new() -> Self {
        QbeContext::default()
    }

    // global variable definitions
    pub fn global<T: for<'a> Into<QbeData<'a>>>(&mut self, val: T) -> Result<QbeValue> {
        let data = val.into();
        let id = self.global_counter;
        write!(&mut self.compiled, "data $_{} = {{ ", id)?;
        data.gen(&mut self.compiled)?;
        self.compiled.write_str(" }\n")?;
        self.global_counter += 1;
        Ok(QbeValue::Global(id))
    }
    pub fn global_at<T: for<'a> Into<QbeData<'a>>>(&mut self, at: QbeForwardDecl, val: T) -> Result<QbeValue> {
        let data = val.into();
        let id = at.0;
        writeln!(&mut self.compiled, "data $_{} = {{ ", id)?;
        data.gen(&mut self.compiled)?;
        self.compiled.write_str(" }\n")?;
        Ok(QbeValue::Global(id))
    }
    pub fn global_ext<T: for<'a> Into<QbeData<'a>>>(&mut self, val: T, opts: &QbeDecl) -> Result<QbeValue> {
        let data = val.into();
        if opts.thread_local {
            writeln!(&mut self.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut self.compiled, "section {} {}", sec, flags)?;
        }
        if let Some(name) = opts.export_as {
            writeln!(&mut self.compiled, "export")?;
            let s = Box::into_pin(Box::<str>::from(name));
            let ptr = s.deref() as *const str;
            self.names.push(s);
            let name = unsafe { ptr.as_ref().unwrap() };
            if let Some(align) = opts.align_to {
                writeln!(&mut self.compiled, "data ${} = align {} {{ ", name, align)?;
            } else {
                writeln!(&mut self.compiled, "data ${} = {{ ", name)?;
            }
            data.gen(&mut self.compiled)?;
            self.compiled.write_str(" }\n")?;
            Ok(QbeValue::Named(name))
        } else {
            let id = self.global_counter;
            if let Some(align) = opts.align_to {
                writeln!(&mut self.compiled, "data $_{} = align {} {{ ", id, align)?;
            } else {
                writeln!(&mut self.compiled, "data $_{} = {{ ", id)?;
            }
            data.gen(&mut self.compiled)?;
            self.compiled.write_str(" }\n")?;
            self.global_counter += 1;
            Ok(QbeValue::Global(id))
        }
    }
    pub fn global_at_ext<T: for<'a> Into<QbeData<'a>>>(&mut self, at: QbeForwardDecl, val: T, opts: &QbeDecl) -> Result<QbeValue> {
        let data = val.into();
        let id = at.0;
        if opts.export_as.is_some() {
            return Err(QbeError::ForwardDeclareName);
        }
        if opts.thread_local {
            writeln!(&mut self.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut self.compiled, "section {} {}", sec, flags)?;
        }
        if let Some(align) = opts.align_to {
            writeln!(&mut self.compiled, "data $_{} = align {} {{ ", id, align)?;
        } else {
            writeln!(&mut self.compiled, "data $_{} = {{ ", id)?;
        }
        data.gen(&mut self.compiled)?;
        self.compiled.write_str(" }\n")?;
        Ok(QbeValue::Global(id))
    }
    pub fn global_zeroed(&mut self, size: u64) -> Result<QbeValue> {
        let id = self.global_counter;
        writeln!(&mut self.compiled, "data $_{} = {{ z {} }}", id, size)?;
        self.global_counter += 1;
        Ok(QbeValue::Global(id))
    }
    pub fn global_zeroed_at(&mut self, at: QbeForwardDecl, size: u64) -> Result<QbeValue> {
        let id = at.0;
        writeln!(&mut self.compiled, "data $_{} = {{ z {} }}", id, size)?;
        Ok(QbeValue::Global(id))
    }
    pub fn global_zeroed_ext(&mut self, size: u64, opts: &QbeDecl) -> Result<QbeValue> {
        if opts.thread_local {
            writeln!(&mut self.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut self.compiled, "section {} {}", sec, flags)?;
        }
        if let Some(name) = opts.export_as {
            writeln!(&mut self.compiled, "export")?;
            let s = Box::into_pin(Box::<str>::from(name));
            let ptr = s.deref() as *const str;
            self.names.push(s);
            let name = unsafe { ptr.as_ref().unwrap() };
            if let Some(align) = opts.align_to {
                writeln!(&mut self.compiled, "data ${} = align {} {{ z {} }}", name, align, size)?;
            } else {
                writeln!(&mut self.compiled, "data ${} = {{ z {} }}", name, size)?;
            }
            Ok(QbeValue::Named(name))
        } else {
            let id = self.global_counter;
            if let Some(align) = opts.align_to {
                writeln!(&mut self.compiled, "data $_{} = align {} {{ z {} }}", id, align, size)?;
            } else {
                writeln!(&mut self.compiled, "data $_{} = {{ z {} }}", id, size)?;
            }
            self.global_counter += 1;
            Ok(QbeValue::Global(id))
        }
    }
    pub fn global_zeroed_at_ext(&mut self, at: QbeForwardDecl, size: u64, opts: &QbeDecl) -> Result<QbeValue> {
        let id = at.0;
        if opts.export_as.is_some() {
            return Err(QbeError::ForwardDeclareName);
        }
        if opts.thread_local {
            writeln!(&mut self.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut self.compiled, "section {} {}", sec, flags)?;
        }
        if let Some(align) = opts.align_to {
            writeln!(&mut self.compiled, "data $_{} = align {} {{ z {} }}", id, align, size)?;
        } else {
            writeln!(&mut self.compiled, "data $_{} = {{ z {} }}", id, size)?;
        }
        Ok(QbeValue::Global(id))
    }
    pub fn global_symbol(&mut self, sym: &str) -> QbeValue {
        let s = Box::into_pin(Box::<str>::from(sym));
        let ptr = s.deref() as *const str;
        self.names.push(s);
        QbeValue::Named(unsafe { ptr.as_ref().unwrap() })
    }

    // type definitions
    pub fn opaque_type(&mut self, align: u64, size: u64) -> Result<QbeType> {
        let id = self.type_counter;
        writeln!(&mut self.compiled, "type :_{} = align {} {{ {} }}", id, align, size)?;
        self.type_counter += 1;
        Ok(QbeType::UserDefined(id))
    }
    pub fn struct_type(&mut self, members: &[QbeType]) -> Result<QbeType> {
        let id = self.type_counter;
        write!(&mut self.compiled, "type :_{} = {{", id)?;
        for memb in members {
            if !memb.is_numeric() {
                return Err(QbeError::NotBasic);
            }
            write!(&mut self.compiled, "{}, ", memb.basic_name())?;
        }
        self.compiled.write_str("}\n")?;
        self.type_counter += 1;
        Ok(QbeType::UserDefined(id))
    }
    pub fn struct_type_align(&mut self, members: &[QbeType], align: u64) -> Result<QbeType> {
        let id = self.type_counter;
        write!(&mut self.compiled, "type :_{} = align {} {{", id, align)?;
        for memb in members {
            if !memb.is_numeric() {
                return Err(QbeError::NotBasic);
            }
            write!(&mut self.compiled, "{}, ", memb.basic_name())?;
        }
        self.compiled.write_str("}\n")?;
        self.type_counter += 1;
        Ok(QbeType::UserDefined(id))
    }

    // function definition
    fn make_function<'a, const VARIADIC: bool, Out, F>(&'a mut self, name: QbeFunctionInner, params: &'a [QbeType], builder: F) -> Result<Out::UserData>
    where Out: QbeFunctionOutput<'a>, F: FnOnce(&mut QbeFunctionBuilder<Out, VARIADIC>) -> Result<Out> {
        let mut f = QbeFunctionBuilder::<Out, VARIADIC>::new(params, &mut self.names);

        let out = f.build(builder)?;
        out.prep_func(&mut self.compiled)?;

        name.gen(&mut self.compiled)?;
        self.compiled.write_char('(')?;

        let mut count = 0;
        for memb in params {
            // user defined types are passed as pointers
            memb.pointer_ud().gen(&mut self.compiled)?;
            write!(&mut self.compiled, " %_{}, ", count)?;
            count += 1;
        }

        self.compiled.write_str(") {\n")?;
        self.compiled.write_str(&f.compiled)?;
        self.compiled.write_str("}\n")?;
        Ok(out.get_ud())
    }
    pub fn function<'a, Out, F>(&'a mut self, params: &'a [QbeType], builder: F) -> Result<impl QbeFunctionCall<'a>>
    where Out: QbeFunctionOutput<'a>, F: FnOnce(&mut QbeFunctionBuilder<Out, NON_VARIADIC_FUNC>) -> Result<Out> {
        let name = QbeFunctionInner::Global(self.global_counter);
        self.global_counter += 1;
        let ud = self.make_function(name, params, builder)?;
        Ok(QbeFunction::<Out> {
            inner: name,
            ud,
        })
    }
    pub fn function_at<'a, Out, F>(&'a mut self, at: QbeForwardDecl, params: &'a [QbeType], builder: F) -> Result<impl QbeFunctionCall<'a>>
    where Out: QbeFunctionOutput<'a>, F: FnOnce(&mut QbeFunctionBuilder<Out, NON_VARIADIC_FUNC>) -> Result<Out> {
        let name = QbeFunctionInner::Global(at.0);
        let ud = self.make_function(name, params, builder)?;
        Ok(QbeFunction::<Out> {
            inner: name,
            ud,
        })
    }
    pub fn function_ext<'a, Out, F>(&'a mut self, params: &'a [QbeType], opts: &QbeDecl, builder: F) -> Result<impl QbeFunctionCall<'a>>
    where Out: QbeFunctionOutput<'a>, F: FnOnce(&mut QbeFunctionBuilder<Out, NON_VARIADIC_FUNC>) -> Result<Out> {
        let out_name = if let Some(name) = opts.export_as {
            let s = Box::into_pin(Box::<str>::from(name));
            let ptr = s.deref() as *const str;
            self.names.push(s);
            QbeFunctionInner::Named(unsafe { ptr.as_ref().unwrap() })
        } else {
            let id = self.global_counter;
            self.global_counter += 1;
            QbeFunctionInner::Global(id)
        };

        if opts.thread_local {
            writeln!(&mut self.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut self.compiled, "section {} {}", sec, flags)?;
        }
        if opts.export_as.is_some() {
            writeln!(&mut self.compiled, "export")?;
        }

        let ud = self.make_function(out_name, params, builder)?;
        Ok(QbeFunction::<Out> {
            inner: out_name,
            ud,
        })
    }
    pub fn function_ext_at<'a, Out, F>(&'a mut self, at: QbeForwardDecl, params: &'a [QbeType], opts: &QbeDecl, builder: F) -> Result<impl QbeFunctionCall<'a>>
    where Out: QbeFunctionOutput<'a>, F: FnOnce(&mut QbeFunctionBuilder<Out, NON_VARIADIC_FUNC>) -> Result<Out> {
        if opts.export_as.is_some() {
            return Err(QbeError::ForwardDeclareName);
        }

        if opts.thread_local {
            writeln!(&mut self.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut self.compiled, "section {} {}", sec, flags)?;
        }
        if opts.export_as.is_some() {
            writeln!(&mut self.compiled, "export")?;
        }

        let name = QbeFunctionInner::Global(at.0);
        let ud = self.make_function(name, params, builder)?;
        Ok(QbeFunction::<Out> {
            inner: name,
            ud,
        })
    }

    pub fn forward_declare(&mut self) -> QbeForwardDecl {
        let id = self.global_counter;
        self.global_counter += 1;
        QbeForwardDecl(id)
    }

    pub fn compile(self) -> String {
        self.compiled
    }

    pub fn write_assembly_to_file(self, file_name: &str) -> std::result::Result<(), errno::Errno> {
        let f = CFile::open(file_name, "w\0")?;
        write_assembly_to_file(&self.compiled, QbeTarget::default(), &f)?;
        Ok(())
    }
    pub fn to_assembly(self) -> std::result::Result<String, errno::Errno> {
        write_assembly_to_string(&self.compiled, QbeTarget::default())
    }
}
