use std::fmt::Write;
use std::pin::Pin;
use std::ops::Deref;
use crate::{Result, QbeError};
use crate::value::{QbeValue, QbeData, QbeType, QbeForwardDecl, QbeCodegen};
use crate::func::{QbeFunctionBuilder, QbeFunctionParams};

#[derive(Builder, Clone, Debug)]
pub struct QbeDecl<'a> {
    #[builder(setter(strip_option), default)]
    section: Option<&'a str>,
    #[builder(setter(strip_option), default)]
    section_flags: Option<&'a str>,
    #[builder(setter(strip_option), default)]
    thread_local: bool,
    #[builder(setter(strip_option), default)]
    export_as: Option<&'a str>,
    #[builder(setter(strip_option), default)]
    align_to: Option<u64>,
}

#[derive(Clone, Default, Debug)]
pub struct QbeContext {
    global_counter: u32,
    type_counter: u32,
    compiled: String,
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
        self.compiled.push_str(" }\n");
        self.global_counter += 1;
        Ok(QbeValue::Global(id))
    }
    pub fn global_at<T: for<'a> Into<QbeData<'a>>>(&mut self, at: QbeForwardDecl, val: T) -> Result<QbeValue> {
        let data = val.into();
        let id = at.0;
        writeln!(&mut self.compiled, "data $_{} = {{ ", id)?;
        data.gen(&mut self.compiled)?;
        self.compiled.push_str(" }\n");
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
            let slice = unsafe { ptr.as_ref().unwrap() };
            if let Some(align) = opts.align_to {
                writeln!(&mut self.compiled, "data ${} = align {} {{ ", slice, align)?;
            } else {
                writeln!(&mut self.compiled, "data ${} = {{ ", slice)?;
            }
            data.gen(&mut self.compiled)?;
            self.compiled.push_str(" }\n");
            Ok(QbeValue::Named(slice))
        } else {
            let id = self.global_counter;
            if let Some(align) = opts.align_to {
                writeln!(&mut self.compiled, "data $_{} = align {} {{ ", id, align)?;
            } else {
                writeln!(&mut self.compiled, "data $_{} = {{ ", id)?;
            }
            data.gen(&mut self.compiled)?;
            self.compiled.push_str(" }\n");
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
        self.compiled.push_str(" }\n");
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
            let slice = unsafe { ptr.as_ref().unwrap() };
            if let Some(align) = opts.align_to {
                writeln!(&mut self.compiled, "data ${} = align {} {{ z {} }}", slice, align, size)?;
            } else {
                writeln!(&mut self.compiled, "data ${} = {{ z {} }}", slice, size)?;
            }
            Ok(QbeValue::Named(slice))
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
    pub fn global_symbol(&mut self, sym: &str) -> Result<QbeValue> {
        let s = Box::into_pin(Box::<str>::from(sym));
        let ptr = s.deref() as *const str;
        self.names.push(s);
        Ok(QbeValue::Named(unsafe { ptr.as_ref().unwrap() }))
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
        self.compiled.push_str("}\n");
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
        self.compiled.push_str("}\n");
        self.type_counter += 1;
        Ok(QbeType::UserDefined(id))
    }

    // function definition
    pub fn function<'a, F: FnOnce(&mut QbeFunctionBuilder) -> Result<Option<QbeValue<'a>>>>(&'a mut self, params: &'a QbeFunctionParams, builder: F) -> Result<QbeValue> {
        let mut f = QbeFunctionBuilder::new(params, &mut self.names);
        let out_typ = f.build(builder)?;

        let id = self.global_counter;
        if let Some(typ) = out_typ {
            self.compiled.push_str("function ");
            typ.gen(&mut self.compiled)?;
            write!(&mut self.compiled, " $_{}(", id)?;
        } else {
            write!(&mut self.compiled, "function $_{}(", id)?;
        }
        params.gen(&mut self.compiled)?;
        self.compiled.push_str(") {\n");
        self.compiled.push_str(&f.compile());
        self.compiled.push_str("}\n");
        self.global_counter += 1;
        Ok(QbeValue::Global(id))
    }
    pub fn function_at<'a, F: FnOnce(&mut QbeFunctionBuilder) -> Result<Option<QbeValue<'a>>>>(&'a mut self, at: QbeForwardDecl, params: &'a QbeFunctionParams, builder: F) -> Result<QbeValue> {
        let id = at.0;
        let mut f = QbeFunctionBuilder::new(params, &mut self.names);
        let out_typ = f.build(builder)?;

        if let Some(typ) = out_typ {
            self.compiled.push_str("function ");
            typ.gen(&mut self.compiled)?;
            write!(&mut self.compiled, " $_{}(", id)?;
        } else {
            write!(&mut self.compiled, "function $_{}(", id)?;
        }
        params.gen(&mut self.compiled)?;
        self.compiled.push_str(") {\n");
        self.compiled.push_str(&f.compile());
        self.compiled.push_str("}\n");
        Ok(QbeValue::Global(id))
    }
    pub fn function_ext<'a, F: FnOnce(&mut QbeFunctionBuilder) -> Result<Option<QbeValue<'a>>>>(&'a mut self, params: &'a QbeFunctionParams, opts: &QbeDecl, builder: F) -> Result<QbeValue> {
        let out_value = if let Some(name) = opts.export_as {
            let s = Box::into_pin(Box::<str>::from(name));
            let ptr = s.deref() as *const str;
            self.names.push(s);
            let slice = unsafe { ptr.as_ref().unwrap() };
            QbeValue::Named(slice)
        } else {
            let id = self.global_counter;
            self.global_counter += 1;
            QbeValue::Global(id)
        };

        let mut f = QbeFunctionBuilder::new(params, &mut self.names);
        let out_typ = f.build(builder)?;
        let compiled = f.compile();

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
        self.compiled.push_str("function ");
        if let Some(typ) = out_typ {
            typ.gen(&mut self.compiled)?;
            self.compiled.push(' ');
        }
        out_value.gen(&mut self.compiled)?;
        self.compiled.push('(');
        params.gen(&mut self.compiled)?;
        self.compiled.push_str(") {\n");
        self.compiled.push_str(&compiled);
        self.compiled.push_str("}\n");
        Ok(out_value)
    }
    pub fn function_at_ext<'a, F: FnOnce(&mut QbeFunctionBuilder) -> Result<Option<QbeValue<'a>>>>(&'a mut self, at: QbeForwardDecl, params: &'a QbeFunctionParams, opts: &QbeDecl, builder: F) -> Result<QbeValue> {
        let id = at.0;
        if opts.export_as.is_some() {
            return Err(QbeError::ForwardDeclareName);
        }
        let mut f = QbeFunctionBuilder::new(params, &mut self.names);
        let out_typ = f.build(builder)?;

        if opts.thread_local {
            writeln!(&mut self.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut self.compiled, "section {} {}", sec, flags)?;
        }
        if let Some(typ) = out_typ {
            self.compiled.push_str("function ");
            typ.gen(&mut self.compiled)?;
            write!(&mut self.compiled, " $_{}(", id)?;
        } else {
            write!(&mut self.compiled, "function $_{}(", id)?;
        }
        params.gen(&mut self.compiled)?;
        self.compiled.push_str(") {\n");
        self.compiled.push_str(&f.compile());
        self.compiled.push_str("}\n");
        Ok(QbeValue::Global(id))
    }

    pub fn forward_declare(&mut self) -> QbeForwardDecl {
        let id = self.global_counter;
        self.global_counter += 1;
        QbeForwardDecl(id)
    }

    pub fn compile(self) -> String {
        self.compiled
    }

    #[cfg(feature = "qbe-command")]
    pub fn into_assembly(self, target: crate::QbeTarget) -> std::io::Result<String> {
        use std::process::Command;
        use std::io::Write;
        let temp = tempfile::NamedTempFile::new()?;
        let path = temp.as_ref();
        let mut f = temp.reopen()?;
        f.write_all(self.compiled.as_bytes())?;
        unsafe {
            Ok(String::from_utf8_unchecked(
                Command::new("qbe")
                .arg("-t").arg(target.as_str())
                .arg(path)
                .output()?
                .stdout
            ))
        }
    }
}
