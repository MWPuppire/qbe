use std::fmt::{self, Write};
use crate::{Result, QbeError};
use crate::value::{QbeType, QbeValue, QbeLabel, QbeBasicType};

#[derive(Clone, Debug)]
pub struct QbeFunctionParams<'a> {
    env: bool,
    variadic: bool,
    params: &'a [QbeType],
}
impl QbeFunctionParams<'_> {
    fn count(&self) -> u32 {
        (self.params.len() + if self.env { 1 } else { 0 }).try_into().unwrap()
    }
}
impl fmt::Display for QbeFunctionParams<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut count = 0;
        if self.env {
            write!(f, "env %_0, ")?;
            count += 1;
        }
        for memb in self.params {
            write!(f, "{} %_{}, ", memb, count)?;
            count += 1;
        }
        if self.variadic {
            write!(f, "...")?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct QbeFunctionBuilder {
    params: Vec<QbeType>,
    env: bool,
    variadic: bool,
    local_counter: u32,
    block_counter: u32,
    compiled: String,
    names: String,
    ret: Option<QbeType>,
}
impl QbeFunctionBuilder {
    pub(crate) fn new(params: &QbeFunctionParams, ret: Option<QbeType>) -> Self {
        QbeFunctionBuilder {
            // user defined types are passed as pointers
            params: params.params.iter().map(|x| x.pointer_ud()).collect(),
            env: params.env,
            variadic: params.variadic,
            local_counter: params.count(),
            block_counter: 0,
            compiled: String::from("@start\n"),
            names: String::new(),
            ret,
        }
    }

    pub fn env(&self) -> Result<QbeValue> {
        if !self.env {
            Err(QbeError::NoEnvArgument)
        } else {
            Ok(QbeValue::Temporary(QbeType::Long, 0))
        }
    }
    pub fn argument(&self, idx: usize) -> Result<QbeValue> {
        if self.params.len() >= idx {
            Err(QbeError::ArgumentOutOfBounds)
        } else {
            let idx = idx + if self.env { 1 } else { 0 };
            Ok(QbeValue::Temporary(self.params[idx], idx.try_into().unwrap()))
        }
    }

    pub fn global_symbol(&mut self, sym: &str) -> Result<QbeValue> {
        let start = self.names.len();
        self.names.push('$');
        self.names.push_str(sym);
        let slice = &self.names[start..self.names.len()];
        Ok(QbeValue::Named(slice))
    }
    pub fn thread_local_symbol(&mut self, sym: &str) -> Result<QbeValue> {
        let start = self.names.len();
        self.names.push_str("thread $");
        self.names.push_str(sym);
        let slice = &self.names[start..self.names.len()];
        Ok(QbeValue::Named(slice))
    }

    pub fn block(&mut self) -> Result<QbeLabel> {
        let id = self.block_counter;
        writeln!(&mut self.compiled, "@_{}", id)?;
        self.block_counter += 1;
        Ok(QbeLabel(id))
    }

    // phi
    pub fn phi<X, Y>(&mut self, path1: QbeLabel, val1: X, path2: QbeLabel, val2: Y, t: QbeBasicType) -> Result<QbeValue>
    where X: for<'a> Into<QbeValue<'a>>, Y: for<'a> Into<QbeValue<'a>> {
        let id = self.local_counter;
        let t = t.promote();
        writeln!(&mut self.compiled, "%_{} ={} @_{} {}, @_{} {}",
            id, t, path1.0, val1.into(), path2.0, val2.into()
        )?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t.into(), id))
    }

    // call
    pub fn call<F, I, A>(&mut self, func: F, args: I) -> Result<()>
    where F: for<'a> Into<QbeValue<'a>>, I: IntoIterator<Item = A>, A: for<'a> Into<QbeValue<'a>> {
        let func = func.into();
        if !func.is_global() {
            return Err(QbeError::NonGlobalCall);
        }
        write!(&mut self.compiled, "call {}(", func)?;
        for arg in args {
            let arg = arg.into();
            write!(&mut self.compiled, "{} {}, ", arg.type_of().pointer_ud(), arg)?;
        }
        writeln!(&mut self.compiled, ")")?;
        Ok(())
    }
    pub fn call_ret<F, I, A>(&mut self, func: F, args: I, t: QbeType) -> Result<QbeValue>
    where F: for<'a> Into<QbeValue<'a>>, I: IntoIterator<Item = A>, A: for<'a> Into<QbeValue<'a>> {
        let func = func.into();
        if !func.is_global() {
            return Err(QbeError::NonGlobalCall);
        }
        let id = self.local_counter;
        write!(&mut self.compiled, "%_{} ={} call {}(", id, t, func)?;
        for arg in args {
            let arg = arg.into();
            write!(&mut self.compiled, "{} {}, ", arg.type_of().pointer_ud(), arg)?;
        }
        writeln!(&mut self.compiled, ")")?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t, id))
    }
    pub fn call_env<F, E, I, A>(&mut self, func: F, env: E, args: I) -> Result<()>
    where F: for<'a> Into<QbeValue<'a>>, E: for<'a> Into<QbeValue<'a>>, I: IntoIterator<Item = A>, A: for<'a> Into<QbeValue<'a>> {
        let func = func.into();
        if !func.is_global() {
            return Err(QbeError::NonGlobalCall);
        }
        write!(&mut self.compiled, "call {}(env {}, ", func, env.into())?;
        for arg in args {
            let arg = arg.into();
            write!(&mut self.compiled, "{} {}, ", arg.type_of().pointer_ud(), arg)?;
        }
        writeln!(&mut self.compiled, ")")?;
        Ok(())
    }
    pub fn call_env_ret<F, E, I, A>(&mut self, func: F, env: E, args: I, t: QbeType) -> Result<QbeValue>
    where F: for<'a> Into<QbeValue<'a>>, E: for<'a> Into<QbeValue<'a>>, I: IntoIterator<Item = A>, A: for<'a> Into<QbeValue<'a>> {
        let func = func.into();
        if !func.is_global() {
            return Err(QbeError::NonGlobalCall);
        }
        let id = self.local_counter;
        write!(&mut self.compiled, "%_{} ={} call {}({}, ", id, t, func, env.into())?;
        for arg in args {
            let arg = arg.into();
            write!(&mut self.compiled, "{} {}, ", arg.type_of().pointer_ud(), arg)?;
        }
        writeln!(&mut self.compiled, ")")?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t, id))
    }
    pub fn call_va<F, I, A>(&mut self, func: F, args: I, va_args: I) -> Result<()>
    where F: for<'a> Into<QbeValue<'a>>, I: IntoIterator<Item = A>, A: for<'a> Into<QbeValue<'a>> {
        let func = func.into();
        if !func.is_global() {
            return Err(QbeError::NonGlobalCall);
        }
        write!(&mut self.compiled, "call {}(", func)?;
        for arg in args {
            let arg = arg.into();
            write!(&mut self.compiled, "{} {}, ", arg.type_of().pointer_ud(), arg)?;
        }
        write!(&mut self.compiled, "..., ")?;
        for arg in va_args {
            let arg = arg.into();
            write!(&mut self.compiled, "{} {}, ", arg.type_of().pointer_ud(), arg)?;
        }
        writeln!(&mut self.compiled, ")")?;
        Ok(())
    }
    pub fn call_va_ret<F, I, A>(&mut self, func: F, args: I, va_args: I, t: QbeType) -> Result<QbeValue>
    where F: for<'a> Into<QbeValue<'a>>, I: IntoIterator<Item = A>, A: for<'a> Into<QbeValue<'a>> {
        let func = func.into();
        if !func.is_global() {
            return Err(QbeError::NonGlobalCall);
        }
        let id = self.local_counter;
        write!(&mut self.compiled, "%_{} ={} call {}(", id, t, func)?;
        for arg in args {
            let arg = arg.into();
            write!(&mut self.compiled, "{} {}, ", arg.type_of().pointer_ud(), arg)?;
        }
        write!(&mut self.compiled, "..., ")?;
        for arg in va_args {
            let arg = arg.into();
            write!(&mut self.compiled, "{} {}, ", arg.type_of().pointer_ud(), arg)?;
        }
        writeln!(&mut self.compiled, ")")?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t, id))
    }
    pub fn call_va_env<F, E, I, A>(&mut self, func: F, env: E, va_args: I, args: I) -> Result<()>
    where F: for<'a> Into<QbeValue<'a>>, E: for<'a> Into<QbeValue<'a>>, I: IntoIterator<Item = A>, A: for<'a> Into<QbeValue<'a>> {
        let func = func.into();
        if !func.is_global() {
            return Err(QbeError::NonGlobalCall);
        }
        write!(&mut self.compiled, "call {}(env {}, ", func, env.into())?;
        for arg in args {
            let arg = arg.into();
            write!(&mut self.compiled, "{} {}, ", arg.type_of().pointer_ud(), arg)?;
        }
        write!(&mut self.compiled, "..., ")?;
        for arg in va_args {
            let arg = arg.into();
            write!(&mut self.compiled, "{} {}, ", arg.type_of().pointer_ud(), arg)?;
        }
        writeln!(&mut self.compiled, ")")?;
        Ok(())
    }
    pub fn call_va_env_ret<F, E, I, A>(&mut self, func: F, env: E, args: I, va_args: I, t: QbeType) -> Result<QbeValue>
    where F: for<'a> Into<QbeValue<'a>>, E: for<'a> Into<QbeValue<'a>>, I: IntoIterator<Item = A>, A: for<'a> Into<QbeValue<'a>> {
        let func = func.into();
        if !func.is_global() {
            return Err(QbeError::NonGlobalCall);
        }
        let id = self.local_counter;
        write!(&mut self.compiled, "%_{} ={} call {}(env {}, ", id, t, func, env.into())?;
        for arg in args {
            let arg = arg.into();
            write!(&mut self.compiled, "{} {}, ", arg.type_of().pointer_ud(), arg)?;
        }
        write!(&mut self.compiled, "..., ")?;
        for arg in va_args {
            let arg = arg.into();
            write!(&mut self.compiled, "{} {}, ", arg.type_of().pointer_ud(), arg)?;
        }
        writeln!(&mut self.compiled, ")")?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t, id))
    }

    // arithmetic instructions

    // comparison instructions

    // conversion instructions

    // copy and cast
    pub fn copy<T: for<'a> Into<QbeValue<'a>>>(&mut self, val: T) -> Result<QbeValue> {
        let val = val.into();
        let id = self.local_counter;
        let typ = val.type_of().promote().pointer_ud();
        writeln!(&mut self.compiled, "%_{} ={} copy {}", id, typ, val)?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(typ, id))
    }
    pub fn cast<T: for<'a> Into<QbeValue<'a>>>(&mut self, val: T) -> Result<QbeValue> {
        let val = val.into();
        let typ = val.type_of();
        if !typ.is_numeric() {
            return Err(QbeError::IncorrectType("numeric"));
        }
        let id = self.local_counter;
        let typ = match typ.promote() {
            QbeType::Word => {
                writeln!(&mut self.compiled, "%_{} =s cast {}", id, val)?;
                QbeType::Single
            },
            QbeType::Long => {
                writeln!(&mut self.compiled, "%_{} =d cast {}", id, val)?;
                QbeType::Double
            },
            QbeType::Single => {
                writeln!(&mut self.compiled, "%_{} =w cast {}", id, val)?;
                QbeType::Word
            },
            QbeType::Double => {
                writeln!(&mut self.compiled, "%_{} =l cast {}", id, val)?;
                QbeType::Long
            },
            _ => unreachable!(),
        };
        self.local_counter += 1;
        Ok(QbeValue::Temporary(typ, id))
    }

    // memory instructions
    pub fn store<X, Y>(&mut self, val: X, to: Y) -> Result<()>
    where X: for<'a> Into<QbeValue<'a>>, Y: for<'a> Into<QbeValue<'a>> {
        let to = to.into();
        if !to.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        let val = val.into();
        let typ = match val.type_of() {
            QbeType::Word           => "w",
            QbeType::Long           => "l",
            QbeType::Single         => "s",
            QbeType::Double         => "d",
            QbeType::Byte           => "b",
            QbeType::Half           => "h",
            QbeType::SignedByte     => "b",
            QbeType::SignedHalf     => "h",
            QbeType::UserDefined(_) => "l",
        };
        writeln!(&mut self.compiled, "store{} {} {}", typ, val, to)?;
        Ok(())
    }
    pub fn store_t<X, Y>(&mut self, val: X, to: Y, t: QbeBasicType) -> Result<()>
    where X: for<'a> Into<QbeValue<'a>>, Y: for<'a> Into<QbeValue<'a>> {
        let to = to.into();
        if !to.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        let val = val.into();
        writeln!(&mut self.compiled, "store{} {} {}", t, val, to)?;
        Ok(())
    }
    pub fn load<T: for<'a> Into<QbeValue<'a>>>(&mut self, from: T, t: QbeBasicType) -> Result<QbeValue> {
        let id = self.local_counter;
        let from = from.into();
        if !from.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        match t {
            QbeBasicType::Word   => writeln!(&mut self.compiled, "%_{} =w loaduw {}", id, from)?,
            QbeBasicType::Long   => writeln!(&mut self.compiled, "%_{} =l loadl {}", id, from)?,
            QbeBasicType::Single => writeln!(&mut self.compiled, "%_{} =s loads {}", id, from)?,
            QbeBasicType::Double => writeln!(&mut self.compiled, "%_{} =d loadd {}", id, from)?,
            QbeBasicType::Byte   => writeln!(&mut self.compiled, "%_{} =w loadub {}", id, from)?,
            QbeBasicType::Half   => writeln!(&mut self.compiled, "%_{} =w loaduh {}", id, from)?,
        };
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t.promote().into(), id))
    }
    pub fn load_signed<T: for<'a> Into<QbeValue<'a>>>(&mut self, from: T, t: QbeBasicType) -> Result<QbeValue> {
        let id = self.local_counter;
        let from = from.into();
        if !from.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        match t {
            QbeBasicType::Word   => writeln!(&mut self.compiled, "%_{} =w loadsw {}", id, from)?,
            QbeBasicType::Long   => writeln!(&mut self.compiled, "%_{} =l loadl {}", id, from)?,
            QbeBasicType::Single => writeln!(&mut self.compiled, "%_{} =s loads {}", id, from)?,
            QbeBasicType::Double => writeln!(&mut self.compiled, "%_{} =d loadd {}", id, from)?,
            QbeBasicType::Byte   => writeln!(&mut self.compiled, "%_{} =w loadsb {}", id, from)?,
            QbeBasicType::Half   => writeln!(&mut self.compiled, "%_{} =w loadsh {}", id, from)?,
        };
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t.promote().into(), id))
    }
    pub fn blit<X, Y, Z>(&mut self, src: X, dst: Y, size: Z) -> Result<()>
    where X: for<'a> Into<QbeValue<'a>>, Y: for<'a> Into<QbeValue<'a>>, Z: for<'a> Into<QbeValue<'a>> {
        let src = src.into();
        if !src.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        let dst = dst.into();
        if !dst.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        let size = size.into();
        if !size.type_of().is_integer() {
            return Err(QbeError::IncorrectType("integer"));
        }
        writeln!(&mut self.compiled, "blit {} {} {}", src, dst, size)?;
        Ok(())
    }
    pub fn alloc4<T: for<'a> Into<QbeValue<'a>>>(&mut self, size: T) -> Result<QbeValue> {
        let size = size.into();
        if !size.type_of().is_integer() {
            return Err(QbeError::IncorrectType("integer"));
        }
        let id = self.local_counter;
        writeln!(&mut self.compiled, "%_{} =l alloc4 {}", id, size)?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(QbeType::Long, id))
    }
    pub fn alloc8<T: for<'a> Into<QbeValue<'a>>>(&mut self, size: T) -> Result<QbeValue> {
        let size = size.into();
        if !size.type_of().is_integer() {
            return Err(QbeError::IncorrectType("integer"));
        }
        let id = self.local_counter;
        writeln!(&mut self.compiled, "%_{} =l alloc8 {}", id, size)?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(QbeType::Long, id))
    }
    pub fn alloc16<T: for<'a> Into<QbeValue<'a>>>(&mut self, size: T) -> Result<QbeValue> {
        let size = size.into();
        if !size.type_of().is_integer() {
            return Err(QbeError::IncorrectType("integer"));
        }
        let id = self.local_counter;
        writeln!(&mut self.compiled, "%_{} =l alloc16 {}", id, size)?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(QbeType::Long, id))
    }

    // jumps
    pub fn jmp(&mut self, to: QbeLabel) -> Result<()> {
        writeln!(&mut self.compiled, "jmp @_{}", to.0)?;
        Ok(())
    }
    pub fn jnz<T: for<'a> Into<QbeValue<'a>>>(&mut self, cond: T, yes: QbeLabel, no: QbeLabel) -> Result<()> {
        writeln!(&mut self.compiled, "jnz {}, @_{}, @_{}", cond.into(), yes.0, no.0)?;
        Ok(())
    }
    pub fn ret(&mut self) -> Result<()> {
        if let Some(ret) = self.ret {
            return Err(QbeError::IncorrectReturn(ret));
        }
        writeln!(&mut self.compiled, "ret")?;
        Ok(())
    }
    pub fn ret_val<T: for<'a> Into<QbeValue<'a>>>(&mut self, value: T) -> Result<()> {
        let Some(ret) = self.ret else { return Err(QbeError::CannotReturnValue) };
        let out = value.into();
        if out.type_of() != ret {
            return Err(QbeError::IncorrectReturn(ret));
        }
        writeln!(&mut self.compiled, "ret {}", out)?;
        Ok(())
    }
    pub fn hlt(&mut self) -> Result<()> {
        writeln!(&mut self.compiled, "hlt")?;
        Ok(())
    }

    // variadic arguments
    pub fn vastart<T: for<'a> Into<QbeValue<'a>>>(&mut self, at: T) -> Result<()> {
        if !self.variadic {
            return Err(QbeError::NonVariadic);
        }
        let at = at.into();
        if !at.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        writeln!(&mut self.compiled, "vastart {}", at)?;
        Ok(())
    }
    pub fn vaarg<T: for<'a> Into<QbeValue<'a>>>(&mut self, from: T, t: QbeBasicType) -> Result<QbeValue> {
        if !self.variadic {
            return Err(QbeError::NonVariadic);
        }
        let from = from.into();
        if !from.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        let id = self.local_counter;
        let t = t.promote();
        writeln!(&mut self.compiled, "%_{} ={} vaarg {}", id, t, from)?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t.into(), id))
    }

    pub(crate) fn compile(self) -> String {
        self.compiled
    }
}
