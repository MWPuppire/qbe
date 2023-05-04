use std::fmt::{self, Write};
use crate::{Result, QbeError};
use crate::value::{QbeType, QbeValue, QbeLabel, QbeBasicType};
use paste::paste;

#[derive(Clone, Debug, Default)]
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

macro_rules! unop {
    ($name:ident, $input:ident, $valid:ident, $outtype:expr) => {
        paste! {
            pub fn $name<T: Into<QbeValue>>(&mut self, val: T) -> Result<QbeValue> {
                let $input = val.into();
                if !$input.type_of().[<is_ $valid>]() {
                    return Err(QbeError::IncorrectType(stringify!($valid)));
                }
                let outtyp = $outtype.promote();
                let id = self.local_counter;
                writeln!(&mut self.compiled,
                    concat!("%_{} ={} ", stringify!($name), " {}"),
                    id, outtyp, $input)?;
                self.local_counter += 1;
                Ok(QbeValue::Temporary(outtyp, id))
            }
        }
    };

    ($name:ident, $input:ident, $valid:ident, $outtype:expr, $op_name:ident) => {
        paste! {
            pub fn $name<T: Into<QbeValue>>(&mut self, val: T) -> Result<QbeValue> {
                let $input = val.into();
                if !$input.type_of().[<is_ $valid>]() {
                    return Err(QbeError::IncorrectType(stringify!($valid)));
                }
                let outtyp = $outtype.promote();
                let id = self.local_counter;
                writeln!(&mut self.compiled,
                    concat!("%_{} ={} ", stringify!($op_name), " {}"),
                    id, outtyp, $input)?;
                self.local_counter += 1;
                Ok(QbeValue::Temporary(outtyp, id))
            }
        }
    };
}

macro_rules! binop {
    ($name:ident, $in1:ident, $in2:ident, $valid:ident, $outtype:expr) => {
        paste! {
            pub fn $name<X, Y>(&mut self, val1: X, val2: Y) -> Result<QbeValue>
            where X: Into<QbeValue>, Y: Into<QbeValue> {
                let $in1 = val1.into();
                let $in2 = val2.into();
                if !$in1.type_of().[<is_ $valid>]() {
                    return Err(QbeError::IncorrectType(stringify!($valid)));
                }
                if !$in2.type_of().[<is_ $valid>]() {
                    return Err(QbeError::IncorrectType(stringify!($valid)));
                }
                let outtyp = $outtype.promote();
                let id = self.local_counter;
                writeln!(&mut self.compiled,
                    concat!("%_{} ={} ", stringify!($name), " {}, {}"),
                    id, outtyp, $in1, $in2)?;
                self.local_counter += 1;
                Ok(QbeValue::Temporary(outtyp, id))
            }
        }
    };

    ($name:ident, $in1:ident, $in2:ident, $valid:ident, $outtype:expr, $op_name:ident) => {
        paste! {
            pub fn $name<X, Y>(&mut self, val1: X, val2: Y) -> Result<QbeValue>
            where X: Into<QbeValue>, Y: Into<QbeValue> {
                let $in1 = val1.into();
                let $in2 = val2.into();
                if !$in1.type_of().[<is_ $valid>]() {
                    return Err(QbeError::IncorrectType(stringify!($valid)));
                }
                if !$in2.type_of().[<is_ $valid>]() {
                    return Err(QbeError::IncorrectType(stringify!($valid)));
                }
                let outtyp = $outtype.promote();
                let id = self.local_counter;
                writeln!(&mut self.compiled,
                    concat!("%_{} ={} ", stringify!($op_name), " {}, {}"),
                    id, outtyp, $in1, $in2)?;
                self.local_counter += 1;
                Ok(QbeValue::Temporary(outtyp, id))
            }
        }
    };
}

macro_rules! cmp_op {
    ($name:ident, $valid:ident) => {
        paste! {
            pub fn [<cmp_ $name>]<X, Y>(&mut self, val1: X, val2: Y) -> Result<QbeValue>
            where X: Into<QbeValue>, Y: Into<QbeValue> {
                let val1 = val1.into();
                let val2 = val2.into();
                let common_type = val1.common_type(&val2)?;
                if !common_type.[<is_ $valid>]() {
                    return Err(QbeError::IncorrectType(stringify!($valid)));
                }
                let typ = match common_type {
                    QbeType::Word           => "w",
                    QbeType::Long           => "l",
                    QbeType::Single         => "s",
                    QbeType::Double         => "d",
                    QbeType::Byte           => "w",
                    QbeType::Half           => "w",
                    QbeType::SignedByte     => "w",
                    QbeType::SignedHalf     => "w",
                    QbeType::UserDefined(_) => "l",
                };
                let id = self.local_counter;
                writeln!(&mut self.compiled,
                    concat!("%_{} =l c", stringify!($name), "{} {}, {}"),
                    id, typ, val1, val2)?;
                self.local_counter += 1;
                Ok(QbeValue::Temporary(QbeType::Long, id))
            }
            pub fn [<cmp_ $name w>]<X, Y>(&mut self, val1: X, val2: Y) -> Result<QbeValue>
            where X: Into<QbeValue>, Y: Into<QbeValue> {
                let val1 = val1.into();
                let val2 = val2.into();
                let common_type = val1.common_type(&val2)?;
                if !common_type.[<is_ $valid>]() {
                    return Err(QbeError::IncorrectType(stringify!($valid)));
                }
                let typ = match common_type {
                    QbeType::Word           => "w",
                    QbeType::Long           => "l",
                    QbeType::Single         => "s",
                    QbeType::Double         => "d",
                    QbeType::Byte           => "w",
                    QbeType::Half           => "w",
                    QbeType::SignedByte     => "w",
                    QbeType::SignedHalf     => "w",
                    QbeType::UserDefined(_) => "l",
                };
                let id = self.local_counter;
                writeln!(&mut self.compiled,
                    concat!("%_{} =w c", stringify!($name), "{} {}, {}"),
                    id, typ, val1, val2)?;
                self.local_counter += 1;
                Ok(QbeValue::Temporary(QbeType::Word, id))
            }
        }
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
    returned: bool,
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
            returned: false,
        }
    }

    pub fn start(&self) -> QbeLabel {
        QbeLabel::Start
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

    pub fn initialize(&mut self, typ: QbeType) -> QbeValue {
        let id = self.local_counter;
        self.local_counter += 1;
        QbeValue::Temporary(typ, id)
    }
    pub fn reassign<F: FnOnce(&mut QbeFunctionBuilder) -> Result<QbeValue>>(&mut self, val: QbeValue, mutator: F) -> Result<()> {
        let old_counter = self.local_counter;
        let new_counter = match val {
            QbeValue::Temporary(_, id) => id,
            _ => return Err(QbeError::NonLocalRedefinition),
        };
        self.local_counter = new_counter;
        let out = mutator(self)?;
        match out {
            QbeValue::Temporary(_, id) => if id != new_counter {
                return Err(QbeError::ReassignmentSingleExpr);
            },
            _ => return Err(QbeError::ReassignmentSingleExpr),
        };
        self.local_counter = old_counter;
        Ok(())
    }

    pub fn global_symbol(&mut self, sym: &str) -> Result<QbeValue> {
        let start = self.names.len();
        self.names.push('$');
        self.names.push_str(sym);
        let slice = &self.names[start..self.names.len()];
        Ok(QbeValue::Named(unsafe { std::mem::transmute(slice) }))
    }
    pub fn thread_local_symbol(&mut self, sym: &str) -> Result<QbeValue> {
        let start = self.names.len();
        self.names.push_str("thread $");
        self.names.push_str(sym);
        let slice = &self.names[start..self.names.len()];
        Ok(QbeValue::Named(unsafe { std::mem::transmute(slice) }))
    }

    pub fn block(&mut self) -> Result<QbeLabel> {
        let id = self.block_counter;
        writeln!(&mut self.compiled, "@_{}", id)?;
        self.block_counter += 1;
        Ok(QbeLabel::Actual(id))
    }
    pub fn block_at(&mut self, at: QbeLabel) -> Result<QbeLabel> {
        let id = match at {
            QbeLabel::ForwardDeclare(id) => id,
            _ => return Err(QbeError::AlreadyDefined),
        };
        writeln!(&mut self.compiled, "@_{}", id)?;
        Ok(QbeLabel::Actual(id))
    }
    pub fn forward_declare_block(&mut self) -> QbeLabel {
        let id = self.block_counter;
        self.block_counter += 1;
        QbeLabel::ForwardDeclare(id)
    }

    // phi
    pub fn phi<X, Y>(&mut self, path1: QbeLabel, val1: X, path2: QbeLabel, val2: Y, t: QbeBasicType) -> Result<QbeValue>
    where X: Into<QbeValue>, Y: Into<QbeValue> {
        let id = self.local_counter;
        let t = t.promote();
        writeln!(&mut self.compiled, "%_{} ={} phi {} {}, {} {}",
            id, t, path1, val1.into(), path2, val2.into()
        )?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t.into(), id))
    }

    // call
    pub fn call<F, I, A>(&mut self, func: F, args: I) -> Result<()>
    where F: Into<QbeValue>, I: IntoIterator<Item = A>, A: Into<QbeValue> {
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
    where F: Into<QbeValue>, I: IntoIterator<Item = A>, A: Into<QbeValue> {
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
    where F: Into<QbeValue>, E: Into<QbeValue>, I: IntoIterator<Item = A>, A: Into<QbeValue> {
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
    where F: Into<QbeValue>, E: Into<QbeValue>, I: IntoIterator<Item = A>, A: Into<QbeValue> {
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
    where F: Into<QbeValue>, I: IntoIterator<Item = A>, A: Into<QbeValue> {
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
    where F: Into<QbeValue>, I: IntoIterator<Item = A>, A: Into<QbeValue> {
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
    where F: Into<QbeValue>, E: Into<QbeValue>, I: IntoIterator<Item = A>, A: Into<QbeValue> {
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
    where F: Into<QbeValue>, E: Into<QbeValue>, I: IntoIterator<Item = A>, A: Into<QbeValue> {
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
    binop!{add, x, y, numeric, x.common_type(&y)?}
    binop!{sub, x, y, numeric, x.common_type(&y)?}
    binop!{div, x, y, numeric, x.common_type(&y)?}
    binop!{mul, x, y, numeric, x.common_type(&y)?}
    unop!{neg, val, numeric, val.type_of()}
    binop!{udiv, x, y, integer, x.common_type(&y)?}
    binop!{rem, x, y, integer, x.common_type(&y)?}
    binop!{urem, x, y, integer, x.common_type(&y)?}
    binop!{or, x, y, integer, x.common_type(&y)?}
    binop!{xor, x, y, integer, x.common_type(&y)?}
    binop!{and, x, y, integer, x.common_type(&y)?}
    binop!{sar, x, y, integer, x.type_of()}
    binop!{shr, x, y, integer, x.type_of()}
    binop!{shl, x, y, integer, x.type_of()}

    // comparison instructions
    cmp_op!{eq, any}
    cmp_op!{ne, any}
    cmp_op!{sle, integer}
    cmp_op!{slt, integer}
    cmp_op!{sge, integer}
    cmp_op!{sgt, integer}
    cmp_op!{ule, integer}
    cmp_op!{ult, integer}
    cmp_op!{uge, integer}
    cmp_op!{ugt, integer}
    cmp_op!{le, floating}
    cmp_op!{lt, floating}
    cmp_op!{ge, floating}
    cmp_op!{gt, floating}
    cmp_op!{o, floating}
    cmp_op!{uo, floating}

    // conversion instructions
    unop!{extsw, val, integer, QbeType::Long}
    unop!{extuw, val, integer, QbeType::Long}
    unop!{extsh, val, integer, QbeType::Long}
    unop!{extuh, val, integer, QbeType::Long}
    unop!{extsb, val, integer, QbeType::Long}
    unop!{extub, val, integer, QbeType::Long}
    unop!{extshw, val, integer, QbeType::Word, extsh}
    unop!{extuhw, val, integer, QbeType::Word, extuh}
    unop!{extsbw, val, integer, QbeType::Word, extsb}
    unop!{extubw, val, integer, QbeType::Word, extub}
    unop!{exts, val, floating, QbeType::Double}
    unop!{truncd, val, floating, QbeType::Single}
    unop!{stosi, val, single, QbeType::Long}
    unop!{stoui, val, single, QbeType::Long}
    unop!{dtosi, val, double, QbeType::Long}
    unop!{dtoui, val, double, QbeType::Long}
    unop!{stosiw, val, single, QbeType::Word, stosi}
    unop!{stouiw, val, single, QbeType::Word, stoui}
    unop!{dtosiw, val, double, QbeType::Word, dtosi}
    unop!{dtouiw, val, double, QbeType::Word, dtoui}
    unop!{swtof, val, word, QbeType::Double}
    unop!{uwtof, val, word, QbeType::Double}
    unop!{sltof, val, long, QbeType::Double}
    unop!{ultof, val, long, QbeType::Double}
    unop!{swtofs, val, word, QbeType::Single, swtof}
    unop!{uwtofs, val, word, QbeType::Single, uwtof}
    unop!{sltofs, val, long, QbeType::Single, sltof}
    unop!{ultofs, val, long, QbeType::Single, ultof}

    // copy and cast
    unop!{copy, val, any, val.type_of()}
    unop!{cast, val, numeric, val.type_of().promote().cast()}

    // memory instructions
    pub fn store<X, Y>(&mut self, val: X, to: Y) -> Result<()>
    where X: Into<QbeValue>, Y: Into<QbeValue> {
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
        writeln!(&mut self.compiled, "store{} {}, {}", typ, val, to)?;
        Ok(())
    }
    pub fn store_t<X, Y>(&mut self, val: X, to: Y, t: QbeBasicType) -> Result<()>
    where X: Into<QbeValue>, Y: Into<QbeValue> {
        let to = to.into();
        if !to.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        let val = val.into();
        writeln!(&mut self.compiled, "store{} {}, {}", t, val, to)?;
        Ok(())
    }
    pub fn load<T: Into<QbeValue>>(&mut self, from: T, t: QbeBasicType) -> Result<QbeValue> {
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
    pub fn load_signed<T: Into<QbeValue>>(&mut self, from: T, t: QbeBasicType) -> Result<QbeValue> {
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
    where X: Into<QbeValue>, Y: Into<QbeValue>, Z: Into<QbeValue> {
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
        writeln!(&mut self.compiled, "blit {}, {}, {}", src, dst, size)?;
        Ok(())
    }
    unop!{alloc4, val, integer, QbeType::Long}
    unop!{alloc8, val, integer, QbeType::Long}
    unop!{alloc16, val, integer, QbeType::Long}

    // jumps
    pub fn jmp(&mut self, to: QbeLabel) -> Result<()> {
        writeln!(&mut self.compiled, "jmp {}", to)?;
        Ok(())
    }
    pub fn jnz<T: Into<QbeValue>>(&mut self, cond: T, yes: QbeLabel, no: QbeLabel) -> Result<()> {
        writeln!(&mut self.compiled, "jnz {}, {}, {}", cond.into(), yes, no)?;
        Ok(())
    }
    pub fn ret(&mut self) -> Result<()> {
        if let Some(ret) = self.ret {
            return Err(QbeError::IncorrectReturn(ret));
        }
        writeln!(&mut self.compiled, "ret")?;
        self.returned = true;
        Ok(())
    }
    pub fn ret_val<T: Into<QbeValue>>(&mut self, value: T) -> Result<()> {
        let Some(ret) = self.ret else { return Err(QbeError::CannotReturnValue) };
        let out = value.into();
        if out.type_of() != ret {
            return Err(QbeError::IncorrectReturn(ret));
        }
        writeln!(&mut self.compiled, "ret {}", out)?;
        self.returned = true;
        Ok(())
    }
    pub fn hlt(&mut self) -> Result<()> {
        writeln!(&mut self.compiled, "hlt")?;
        Ok(())
    }

    // variadic arguments
    pub fn vastart<T: Into<QbeValue>>(&mut self, at: T) -> Result<()> {
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
    pub fn vaarg<T: Into<QbeValue>>(&mut self, from: T, t: QbeBasicType) -> Result<QbeValue> {
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

    pub(crate) fn compile(mut self) -> Result<String> {
        if !self.returned {
            if self.ret.is_some() {
                return Err(QbeError::NoReturn);
            } else {
                self.compiled.push_str("ret");
            }
        }
        Ok(self.compiled)
    }
}
