use std::fmt::{self, Write};
use crate::{Result, QbeError};
use crate::value::{QbeType, QbeValue, QbeLabel, QbeBasicType, QbeForwardLabel, QbeCodegen};
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
impl QbeCodegen for QbeFunctionParams<'_> {
    fn gen(&self, d: &mut dyn Write) -> fmt::Result {
        let mut count = 0;
        if self.env {
            d.write_str("env %_0, ")?;
            count += 1;
        }
        for memb in self.params {
            memb.gen(d)?;
            write!(d, " %_{}, ", count)?;
            count += 1;
        }
        if self.variadic {
            write!(d, "...")?;
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
                let out_name: QbeBasicType = outtyp.into();
                let id = self.local_counter;
                write!(&mut self.compiled,
                    concat!("\t%_{} ={} ", stringify!($name), " "),
                    id, out_name.code_name())?;
                $input.gen(&mut self.compiled)?;
                self.compiled.push_str("\n");
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
                let out_name: QbeBasicType = outtyp.into();
                let id = self.local_counter;
                write!(&mut self.compiled,
                    concat!("\t%_{} ={} ", stringify!($op_name), " "),
                    id, out_name.code_name())?;
                $input.gen(&mut self.compiled)?;
                self.compiled.push_str("\n");
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
                let out_name: QbeBasicType = outtyp.into();
                let id = self.local_counter;
                write!(&mut self.compiled,
                    concat!("\t%_{} ={} ", stringify!($name), " "),
                    id, out_name.code_name())?;
                $in1.gen(&mut self.compiled)?;
                self.compiled.push_str(", ");
                $in2.gen(&mut self.compiled)?;
                self.compiled.push_str("\n");
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
                let out_name: QbeBasicType = outtyp.into();
                let id = self.local_counter;
                write!(&mut self.compiled,
                    concat!("\t%_{} ={} ", stringify!($op_name), " "),
                    id, out_name.code_name())?;
                $in1.gen(&mut self.compiled)?;
                self.compiled.push_str(", ");
                $in2.gen(&mut self.compiled)?;
                self.compiled.push_str("\n");
                self.local_counter += 1;
                Ok(QbeValue::Temporary(outtyp, id))
            }
        }
    };
}

macro_rules! cmp_op {
    ($name:ident, $valid:ident) => {
        paste! {
            pub fn [<$name>]<X, Y>(&mut self, val1: X, val2: Y) -> Result<QbeValue>
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
                write!(&mut self.compiled,
                    concat!("\t%_{} =l c", stringify!($name), "{} "),
                    id, typ)?;
                val1.gen(&mut self.compiled)?;
                self.compiled.push_str(", ");
                val2.gen(&mut self.compiled)?;
                self.compiled.push_str("\n");
                self.local_counter += 1;
                Ok(QbeValue::Temporary(QbeType::Long, id))
            }
            pub fn [<$name _w>]<X, Y>(&mut self, val1: X, val2: Y) -> Result<QbeValue>
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
                write!(&mut self.compiled,
                    concat!("\t%_{} =w c", stringify!($name), "{} "),
                    id, typ)?;
                val1.gen(&mut self.compiled)?;
                self.compiled.push_str(", ");
                val2.gen(&mut self.compiled)?;
                self.compiled.push_str("\n");
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
}
impl QbeFunctionBuilder {
    pub(crate) fn new(params: &QbeFunctionParams) -> Self {
        QbeFunctionBuilder {
            // user defined types are passed as pointers
            params: params.params.iter().map(|x| x.pointer_ud()).collect(),
            env: params.env,
            variadic: params.variadic,
            local_counter: params.count(),
            // 0 is reserved for start block, 1 is reserved for end block
            block_counter: 2,
            // @_0 is the start block
            compiled: String::from("@_0\n"),
            names: String::new(),
        }
    }

    pub fn start(&self) -> QbeLabel {
        QbeLabel(0)
    }

    pub fn end(&self) -> QbeLabel {
        QbeLabel(1)
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
        Ok(QbeLabel(id))
    }
    pub fn block_at(&mut self, at: QbeForwardLabel) -> Result<QbeLabel> {
        let id = at.0;
        writeln!(&mut self.compiled, "@_{}", id)?;
        Ok(QbeLabel(id))
    }
    pub fn forward_declare_block(&mut self) -> QbeForwardLabel {
        let id = self.block_counter;
        self.block_counter += 1;
        QbeForwardLabel(id)
    }

    // phi
    pub fn phi<X, XL, Y, YL>(&mut self, path1: XL, val1: X, path2: YL, val2: Y) -> Result<QbeValue>
    where X: Into<QbeValue>, XL: Into<QbeLabel>, Y: Into<QbeValue>, YL: Into<QbeLabel> {
        let id = self.local_counter;
        let val1 = val1.into();
        let val2 = val2.into();
        let t = val1.common_type(&val2)?;
        let t: QbeBasicType = t.into();
        let t = t.promote();
        write!(&mut self.compiled, "\t%_{} ={} phi @_{} ", id, t.code_name(), path1.into().0)?;
        val1.gen(&mut self.compiled)?;
        write!(&mut self.compiled, ", @_{} ", path2.into().0)?;
        val2.gen(&mut self.compiled)?;
        self.compiled.push_str("\n");
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t.into(), id))
    }
    pub fn phi_t<X, XL, Y, YL>(&mut self, path1: XL, val1: X, path2: YL, val2: Y, t: QbeType) -> Result<QbeValue>
    where X: Into<QbeValue>, XL: Into<QbeLabel>, Y: Into<QbeValue>, YL: Into<QbeLabel> {
        let id = self.local_counter;
        let t: QbeBasicType = t.into();
        let t = t.promote();
        write!(&mut self.compiled, "\t%_{} ={} phi @_{} ", id, t.code_name(), path1.into().0)?;
        val1.into().gen(&mut self.compiled)?;
        write!(&mut self.compiled, ", @_{} ", path2.into().0)?;
        val2.into().gen(&mut self.compiled)?;
        self.compiled.push_str("\n");
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t.into(), id))
    }

    // call
    fn call_inner<I, A>(&mut self, func: QbeValue, env: Option<QbeValue>, args: I, va_args: Option<I>) -> Result<()>
    where I: IntoIterator<Item = A>, A: Into<QbeValue> {
        self.compiled.push_str("call");
        func.gen(&mut self.compiled)?;
        self.compiled.push_str("(");
        if let Some(env) = env {
            self.compiled.push_str("env ");
            env.gen(&mut self.compiled)?;
            self.compiled.push_str(", ");
        }
        for arg in args {
            let arg = arg.into();
            arg.type_of().gen(&mut self.compiled)?;
            self.compiled.push_str(" ");
            arg.gen(&mut self.compiled)?;
            self.compiled.push_str(", ");
        }
        if let Some(va) = va_args {
            self.compiled.push_str("..., ");
            for arg in va {
                let arg = arg.into();
                arg.type_of().gen(&mut self.compiled)?;
                self.compiled.push_str(" ");
                arg.gen(&mut self.compiled)?;
                self.compiled.push_str(", ");
            }
        }
        self.compiled.push_str(")\n");
        Ok(())
    }
    pub fn call<F, I, A>(&mut self, func: F, args: I) -> Result<()>
    where F: Into<QbeValue>, I: IntoIterator<Item = A>, A: Into<QbeValue> {
        let func = func.into();
        if !func.is_global() {
            return Err(QbeError::NonGlobalCall);
        }
        self.compiled.push_str("\t");
        self.call_inner(func, None, args, None)
    }
    pub fn call_ret<F, I, A>(&mut self, func: F, args: I, t: QbeType) -> Result<QbeValue>
    where F: Into<QbeValue>, I: IntoIterator<Item = A>, A: Into<QbeValue> {
        let func = func.into();
        if !func.is_global() {
            return Err(QbeError::NonGlobalCall);
        }
        let id = self.local_counter;
        write!(&mut self.compiled, "\t%_{} =", id)?;
        t.gen(&mut self.compiled)?;
        self.compiled.push_str(" ");
        self.call_inner(func, None, args, None)?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t, id))
    }
    pub fn call_env<F, E, I, A>(&mut self, func: F, env: E, args: I) -> Result<()>
    where F: Into<QbeValue>, E: Into<QbeValue>, I: IntoIterator<Item = A>, A: Into<QbeValue> {
        let func = func.into();
        if !func.is_global() {
            return Err(QbeError::NonGlobalCall);
        }
        self.compiled.push_str("\t");
        self.call_inner(func, Some(env.into()), args, None)
    }
    pub fn call_env_ret<F, E, I, A>(&mut self, func: F, env: E, args: I, t: QbeType) -> Result<QbeValue>
    where F: Into<QbeValue>, E: Into<QbeValue>, I: IntoIterator<Item = A>, A: Into<QbeValue> {
        let func = func.into();
        if !func.is_global() {
            return Err(QbeError::NonGlobalCall);
        }
        let id = self.local_counter;
        write!(&mut self.compiled, "\t%_{} =", id)?;
        t.gen(&mut self.compiled)?;
        self.compiled.push_str(" ");
        self.call_inner(func, Some(env.into()), args, None)?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t, id))
    }
    pub fn call_va<F, I, A>(&mut self, func: F, args: I, va_args: I) -> Result<()>
    where F: Into<QbeValue>, I: IntoIterator<Item = A>, A: Into<QbeValue> {
        let func = func.into();
        if !func.is_global() {
            return Err(QbeError::NonGlobalCall);
        }
        self.compiled.push_str("\t");
        self.call_inner(func, None, args, Some(va_args))
    }
    pub fn call_va_ret<F, I, A>(&mut self, func: F, args: I, va_args: I, t: QbeType) -> Result<QbeValue>
    where F: Into<QbeValue>, I: IntoIterator<Item = A>, A: Into<QbeValue> {
        let func = func.into();
        if !func.is_global() {
            return Err(QbeError::NonGlobalCall);
        }
        let id = self.local_counter;
        write!(&mut self.compiled, "\t%_{} =", id)?;
        t.gen(&mut self.compiled)?;
        self.compiled.push_str(" ");
        self.call_inner(func, None, args, Some(va_args))?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t, id))
    }
    pub fn call_va_env<F, E, I, A>(&mut self, func: F, env: E, va_args: I, args: I) -> Result<()>
    where F: Into<QbeValue>, E: Into<QbeValue>, I: IntoIterator<Item = A>, A: Into<QbeValue> {
        let func = func.into();
        if !func.is_global() {
            return Err(QbeError::NonGlobalCall);
        }
        self.compiled.push_str("\t");
        self.call_inner(func, Some(env.into()), args, Some(va_args))
    }
    pub fn call_va_env_ret<F, E, I, A>(&mut self, func: F, env: E, args: I, va_args: I, t: QbeType) -> Result<QbeValue>
    where F: Into<QbeValue>, E: Into<QbeValue>, I: IntoIterator<Item = A>, A: Into<QbeValue> {
        let func = func.into();
        if !func.is_global() {
            return Err(QbeError::NonGlobalCall);
        }
        let id = self.local_counter;
        write!(&mut self.compiled, "\t%_{} =", id)?;
        t.gen(&mut self.compiled)?;
        self.compiled.push_str(" ");
        self.call_inner(func, Some(env.into()), args, Some(va_args))?;
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
    unop!{extsh_w, val, integer, QbeType::Word, extsh}
    unop!{extuh_w, val, integer, QbeType::Word, extuh}
    unop!{extsb_w, val, integer, QbeType::Word, extsb}
    unop!{extub_w, val, integer, QbeType::Word, extub}
    unop!{exts, val, floating, QbeType::Double}
    unop!{truncd, val, floating, QbeType::Single}
    unop!{stosi, val, single, QbeType::Long}
    unop!{stoui, val, single, QbeType::Long}
    unop!{dtosi, val, double, QbeType::Long}
    unop!{dtoui, val, double, QbeType::Long}
    unop!{stosi_w, val, single, QbeType::Word, stosi}
    unop!{stoui_w, val, single, QbeType::Word, stoui}
    unop!{dtosi_w, val, double, QbeType::Word, dtosi}
    unop!{dtoui_w, val, double, QbeType::Word, dtoui}
    unop!{swtof, val, word, QbeType::Double}
    unop!{uwtof, val, word, QbeType::Double}
    unop!{sltof, val, long, QbeType::Double}
    unop!{ultof, val, long, QbeType::Double}
    unop!{swtof_s, val, word, QbeType::Single, swtof}
    unop!{uwtof_s, val, word, QbeType::Single, uwtof}
    unop!{sltof_s, val, long, QbeType::Single, sltof}
    unop!{ultof_s, val, long, QbeType::Single, ultof}

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
        write!(&mut self.compiled, "\tstore{} ", typ)?;
        val.gen(&mut self.compiled)?;
        self.compiled.push_str(", ");
        to.gen(&mut self.compiled)?;
        self.compiled.push_str("\n");
        Ok(())
    }
    pub fn store_t<X, Y>(&mut self, val: X, to: Y, t: QbeType) -> Result<()>
    where X: Into<QbeValue>, Y: Into<QbeValue> {
        let to = to.into();
        if !to.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        let val = val.into();
        let t: QbeBasicType = t.into();
        write!(&mut self.compiled, "\tstore{} ", t.code_name())?;
        val.gen(&mut self.compiled)?;
        self.compiled.push_str(", ");
        to.gen(&mut self.compiled)?;
        self.compiled.push_str("\n");
        Ok(())
    }
    pub fn load<T: Into<QbeValue>>(&mut self, from: T, t: QbeType) -> Result<QbeValue> {
        let id = self.local_counter;
        let from = from.into();
        if !from.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        let t = t.into();
        match t {
            QbeBasicType::Word   => write!(&mut self.compiled, "\t%_{} =w loaduw ", id)?,
            QbeBasicType::Long   => write!(&mut self.compiled, "\t%_{} =l loadl ", id)?,
            QbeBasicType::Single => write!(&mut self.compiled, "\t%_{} =s loads ", id)?,
            QbeBasicType::Double => write!(&mut self.compiled, "\t%_{} =d loadd ", id)?,
            QbeBasicType::Byte   => write!(&mut self.compiled, "\t%_{} =w loadub ", id)?,
            QbeBasicType::Half   => write!(&mut self.compiled, "\t%_{} =w loaduh ", id)?,
        };
        from.gen(&mut self.compiled)?;
        self.compiled.push_str("\n");
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t.promote().into(), id))
    }
    pub fn load_signed<T: Into<QbeValue>>(&mut self, from: T, t: QbeType) -> Result<QbeValue> {
        let id = self.local_counter;
        let from = from.into();
        if !from.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        let t = t.into();
        match t {
            QbeBasicType::Word   => write!(&mut self.compiled, "\t%_{} =w loadsw ", id)?,
            QbeBasicType::Long   => write!(&mut self.compiled, "\t%_{} =l loadl ", id)?,
            QbeBasicType::Single => write!(&mut self.compiled, "\t%_{} =s loads ", id)?,
            QbeBasicType::Double => write!(&mut self.compiled, "\t%_{} =d loadd ", id)?,
            QbeBasicType::Byte   => write!(&mut self.compiled, "\t%_{} =w loadsb ", id)?,
            QbeBasicType::Half   => write!(&mut self.compiled, "\t%_{} =w loadsh ", id)?,
        };
        from.gen(&mut self.compiled)?;
        self.compiled.push_str("\n");
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
        self.compiled.push_str("\tblit ");
        src.gen(&mut self.compiled)?;
        self.compiled.push_str(", ");
        dst.gen(&mut self.compiled)?;
        self.compiled.push_str(", ");
        size.gen(&mut self.compiled)?;
        self.compiled.push_str("\n");
        Ok(())
    }
    unop!{alloc4, val, integer, QbeType::Long}
    unop!{alloc8, val, integer, QbeType::Long}
    unop!{alloc16, val, integer, QbeType::Long}

    // jumps
    pub fn jmp<T: Into<QbeLabel>>(&mut self, to: T) -> Result<()> {
        writeln!(&mut self.compiled, "\tjmp @_{}", to.into().0)?;
        Ok(())
    }
    pub fn jnz<T,  YL, NL>(&mut self, cond: T, yes: YL, no: NL) -> Result<()>
    where T: Into<QbeValue>, YL: Into<QbeLabel>, NL: Into<QbeLabel> {
        self.compiled.push_str("\tjnz ");
        cond.into().gen(&mut self.compiled)?;
        writeln!(&mut self.compiled, ", @_{}, @_{}", yes.into().0, no.into().0)?;
        Ok(())
    }
    pub fn hlt(&mut self) -> Result<()> {
        self.compiled.push_str("\thlt\n");
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
        self.compiled.push_str("\tvastart ");
        at.gen(&mut self.compiled)?;
        self.compiled.push_str("\n");
        Ok(())
    }
    pub fn vaarg<T: Into<QbeValue>>(&mut self, from: T, t: QbeType) -> Result<QbeValue> {
        if !self.variadic {
            return Err(QbeError::NonVariadic);
        }
        let from = from.into();
        if !from.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        let id = self.local_counter;
        let t: QbeBasicType = t.into();
        let t = t.promote();
        write!(&mut self.compiled, "\t%_{} ={} vaarg ", id, t.code_name())?;
        from.gen(&mut self.compiled)?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t.into(), id))
    }

    pub(crate) fn compile(mut self, ret: Option<QbeValue>) -> Result<String> {
        // label the end block in case anything uses it as a destination
        self.compiled.push_str("@_1\n");
        if let Some(ret) = ret {
            self.compiled.push_str("\tret ");
            ret.gen(&mut self.compiled)?;
            self.compiled.push_str("\n");
        } else {
            self.compiled.push_str("\tret\n");
        }
        Ok(self.compiled)
    }
}
