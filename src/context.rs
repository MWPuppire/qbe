use std::fmt::Write;
use crate::{Result, QbeError};
use crate::value::{QbeValue, QbeData, QbeType, QbeBasicType};
use crate::func::{QbeFunctionBuilder, QbeFunctionParams};

#[cfg(feature = "qbe-command")]
pub enum QbeTarget {
    Amd64,
    Amd64Apple,
    Arm64,
    Arm64Apple,
    RiscV64,
}
#[cfg(feature = "qbe-command")]
impl QbeTarget {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Amd64 => "amd64_sysv",
            Self::Amd64Apple => "amd64_apple",
            Self::Arm64 => "arm64",
            Self::Arm64Apple => "arm64_apple",
            Self::RiscV64 => "rv64",
        }
    }
}

#[derive(Builder, Clone, Debug)]
pub struct QbeGlobalOpts<'a> {
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
    names: String,
}

impl QbeContext {
    pub fn new() -> Self {
        QbeContext::default()
    }

    // global variable definitions
    pub fn global<T: for<'a> Into<QbeData<'a>>>(&mut self, val: T) -> Result<QbeValue> {
        let data = val.into();
        let id = self.global_counter;
        writeln!(&mut self.compiled, "data $_{} = {{ {} }}", id, data)?;
        self.global_counter += 1;
        Ok(QbeValue::Global(id))
    }
    pub fn global_at<T: for<'a> Into<QbeData<'a>>>(&mut self, at: QbeValue, val: T) -> Result<QbeValue> {
        let data = val.into();
        let id = match at {
            QbeValue::ForwardDeclare(id) => id,
            _ => return Err(QbeError::AlreadyDefined),
        };
        writeln!(&mut self.compiled, "data $_{} = {{ {} }}", id, data)?;
        Ok(QbeValue::Global(id))
    }
    pub fn global_ext<T: for<'a> Into<QbeData<'a>>>(&mut self, val: T, opts: &QbeGlobalOpts) -> Result<QbeValue> {
        let data = val.into();
        if opts.thread_local {
            writeln!(&mut self.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut self.compiled, "section {} {}", sec, flags)?;
        }
        if let Some(name) = opts.export_as {
            let start = self.names.len();
            self.names.push('$');
            self.names.push_str(name);
            let slice = &self.names[start..self.names.len()];
            if let Some(align) = opts.align_to {
                writeln!(&mut self.compiled, "data {} = align {} {{ {} }}", slice, align, data)?;
            } else {
                writeln!(&mut self.compiled, "data {} = {{ {} }}", slice, data)?;
            }
            Ok(QbeValue::Named(slice))
        } else {
            let id = self.global_counter;
            if let Some(align) = opts.align_to {
                writeln!(&mut self.compiled, "data $_{} = align {} {{ {} }}", id, align, data)?;
            } else {
                writeln!(&mut self.compiled, "data $_{} = {{ {} }}", id, data)?;
            }
            self.global_counter += 1;
            Ok(QbeValue::Global(id))
        }
    }
    pub fn global_at_ext<T: for<'a> Into<QbeData<'a>>>(&mut self, at: QbeValue, val: T, opts: &QbeGlobalOpts) -> Result<QbeValue> {
        let id = match at {
            QbeValue::ForwardDeclare(id) => id,
            _ => return Err(QbeError::AlreadyDefined),
        };
        if opts.export_as.is_some() {
            return Err(QbeError::ForwardDeclareName);
        }
        let data = val.into();
        if opts.thread_local {
            writeln!(&mut self.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut self.compiled, "section {} {}", sec, flags)?;
        }
        if let Some(align) = opts.align_to {
            writeln!(&mut self.compiled, "data $_{} = align {} {{ {} }}", id, align, data)?;
        } else {
            writeln!(&mut self.compiled, "data $_{} = {{ {} }}", id, data)?;
        }
        Ok(QbeValue::Global(id))
    }
    pub fn global_zeroed(&mut self, size: u64) -> Result<QbeValue> {
        let id = self.global_counter;
        writeln!(&mut self.compiled, "data $_{} = {{ z {} }}", id, size)?;
        self.global_counter += 1;
        Ok(QbeValue::Global(id))
    }
    pub fn global_zeroed_at(&mut self, at: QbeValue, size: u64) -> Result<QbeValue> {
        let id = match at {
            QbeValue::ForwardDeclare(id) => id,
            _ => return Err(QbeError::AlreadyDefined),
        };
        writeln!(&mut self.compiled, "data $_{} = {{ z {} }}", id, size)?;
        Ok(QbeValue::Global(id))
    }
    pub fn global_zeroed_ext(&mut self, size: u64, opts: &QbeGlobalOpts) -> Result<QbeValue> {
        if opts.thread_local {
            writeln!(&mut self.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut self.compiled, "section {} {}", sec, flags)?;
        }
        if let Some(name) = opts.export_as {
            let start = self.names.len();
            self.names.push('$');
            self.names.push_str(name);
            let slice = &self.names[start..self.names.len()];
            if let Some(align) = opts.align_to {
                writeln!(&mut self.compiled, "data {} = align {} {{ z {} }}", slice, align, size)?;
            } else {
                writeln!(&mut self.compiled, "data {} = {{ z {} }}", slice, size)?;
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
    pub fn global_zeroed_at_ext(&mut self, at: QbeValue, size: u64, opts: &QbeGlobalOpts) -> Result<QbeValue> {
        let id = match at {
            QbeValue::ForwardDeclare(id) => id,
            _ => return Err(QbeError::AlreadyDefined),
        };
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
        let start = self.names.len();
        self.names.push('$');
        self.names.push_str(sym);
        let slice = &self.names[start..self.names.len()];
        Ok(QbeValue::Named(slice))
    }

    // type definitions
    pub fn opaque_type(&mut self, align: u64, size: u64) -> Result<QbeType> {
        let id = self.type_counter;
        writeln!(&mut self.compiled, "type :_{} = align {} {{ {} }}", id, align, size)?;
        self.type_counter += 1;
        Ok(QbeType::UserDefined(id))
    }
    pub fn struct_type(&mut self, members: &[QbeBasicType]) -> Result<QbeType> {
        let id = self.type_counter;
        write!(&mut self.compiled, "type :_{} = {{", id)?;
        for memb in members {
            write!(&mut self.compiled, "{}, ", memb)?;
        }
        writeln!(&mut self.compiled, "}}")?;
        self.type_counter += 1;
        Ok(QbeType::UserDefined(id))
    }
    pub fn struct_type_align(&mut self, members: &[QbeBasicType], align: u64) -> Result<QbeType> {
        let id = self.type_counter;
        write!(&mut self.compiled, "type :_{} = align {} {{", id, align)?;
        for memb in members {
            write!(&mut self.compiled, "{}, ", memb)?;
        }
        writeln!(&mut self.compiled, "}}")?;
        self.type_counter += 1;
        Ok(QbeType::UserDefined(id))
    }

    // function definition
    pub fn function<F: FnOnce(&mut QbeFunctionBuilder) -> Result<()>>(&mut self, params: &QbeFunctionParams, ret: Option<QbeType>, builder: F) -> Result<QbeValue> {
        let mut f = QbeFunctionBuilder::new(params, ret);
        builder(&mut f)?;
        let id = self.global_counter;
        if let Some(ret) = ret {
            writeln!(&mut self.compiled, "function {} $_{} ({}) {{", ret, id, params)?;
        } else {
            writeln!(&mut self.compiled, "function $_{} ({}) {{", id, params)?;
        }
        writeln!(&mut self.compiled, "{}", f.compile())?;
        writeln!(&mut self.compiled, "}}")?;
        self.global_counter += 1;
        Ok(QbeValue::Global(id))
    }
    pub fn function_at<F: FnOnce(&mut QbeFunctionBuilder) -> Result<()>>(&mut self, at: QbeValue, params: &QbeFunctionParams, ret: Option<QbeType>, builder: F) -> Result<QbeValue> {
        let id = match at {
            QbeValue::ForwardDeclare(id) => id,
            _ => return Err(QbeError::AlreadyDefined),
        };
        let mut f = QbeFunctionBuilder::new(params, ret);
        builder(&mut f)?;
        if let Some(ret) = ret {
            writeln!(&mut self.compiled, "function {} $_{} ({}) {{", ret, id, params)?;
        } else {
            writeln!(&mut self.compiled, "function $_{} ({}) {{", id, params)?;
        }
        writeln!(&mut self.compiled, "{}", f.compile())?;
        writeln!(&mut self.compiled, "}}")?;
        Ok(QbeValue::Global(id))
    }
    pub fn function_ext<F: FnOnce(&mut QbeFunctionBuilder) -> Result<()>>(&mut self, params: &QbeFunctionParams, ret: Option<QbeType>, opts: &QbeGlobalOpts, builder: F) -> Result<QbeValue> {
        let mut f = QbeFunctionBuilder::new(params, ret);
        builder(&mut f)?;
        if opts.thread_local {
            writeln!(&mut self.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut self.compiled, "section {} {}", sec, flags)?;
        }
        let out_value = if let Some(name) = opts.export_as {
            let start = self.names.len();
            self.names.push('$');
            self.names.push_str(name);
            let slice = &self.names[start..self.names.len()];
            if let Some(ret) = ret {
                writeln!(&mut self.compiled, "function {} {} ({}) {{", ret, slice, params)?;
            } else {
                writeln!(&mut self.compiled, "function {} ({}) {{", slice, params)?;
            }
            QbeValue::Named(slice)
        } else {
            let id = self.global_counter;
            if let Some(ret) = ret {
                writeln!(&mut self.compiled, "function {} $_{} ({}) {{", ret, id, params)?;
            } else {
                writeln!(&mut self.compiled, "function $_{} ({}) {{", id, params)?;
            }
            self.global_counter += 1;
            QbeValue::Global(id)
        };
        writeln!(&mut self.compiled, "{}", f.compile())?;
        writeln!(&mut self.compiled, "}}")?;
        Ok(out_value)
    }
    pub fn function_at_ext<F: FnOnce(&mut QbeFunctionBuilder) -> Result<()>>(&mut self, at: QbeValue, params: &QbeFunctionParams, ret: Option<QbeType>, opts: &QbeGlobalOpts, builder: F) -> Result<QbeValue> {
        let id = match at {
            QbeValue::ForwardDeclare(id) => id,
            _ => return Err(QbeError::AlreadyDefined),
        };
        if opts.export_as.is_some() {
            return Err(QbeError::ForwardDeclareName);
        }
        let mut f = QbeFunctionBuilder::new(params, ret);
        builder(&mut f)?;
        if opts.thread_local {
            writeln!(&mut self.compiled, "thread")?;
        }
        if let Some(sec) = opts.section {
            let flags = opts.section_flags.unwrap_or("");
            writeln!(&mut self.compiled, "section {} {}", sec, flags)?;
        }
        if let Some(ret) = ret {
            writeln!(&mut self.compiled, "function {} $_{} ({}) {{", ret, id, params)?;
        } else {
            writeln!(&mut self.compiled, "function $_{} ({}) {{", id, params)?;
        }
        writeln!(&mut self.compiled, "{}", f.compile())?;
        writeln!(&mut self.compiled, "}}")?;
        Ok(QbeValue::Global(id))
    }

    pub fn forward_declare(&mut self) -> QbeValue {
        let id = self.global_counter;
        self.global_counter += 1;
        QbeValue::ForwardDeclare(id)
    }

    pub fn compile(self) -> String {
        self.compiled
    }

    #[cfg(feature = "qbe-command")]
    pub fn into_assembly(self, target: QbeTarget) -> String {
        use std::fs::File;
        use std::process::Command;
        use std::io::Write;
        use tempfile::tempfile;
        let mut f: File = tempfile().unwrap();
        write!(f, "{}", self.compiled).unwrap();
        String::from_utf8(
            Command::new("qbe")
                .arg("-t").arg(target.as_str())
                .arg("-")
                .stdin(f)
                .output()
                .unwrap()
                .stdout
            ).unwrap()
    }
}
