use crate::value::{QbeCodegen, QbeForwardLabel, QbeFunctionOutput, QbeLabel, QbeType, QbeValue};
use crate::{QbeError, Result};
use paste::paste;
use std::fmt::Write;
use std::pin::Pin;

/// Discriminant for non-variadic functions; used to allow/disallow certain
/// methods in the function builder.
pub const NON_VARIADIC_FUNC: bool = false;
/// Discriminant for variadic functions; used to allow/disallow certain methods
/// in the function builder.
pub const VARIADIC_FUNC: bool = true;

/// Trait for QBE callable objects.
pub trait QbeFunctionCall<'a> {
    type Output: QbeFunctionOutput<'a>;
    fn call_on<Out, I, A, const V: bool>(
        &self,
        caller: &mut QbeFunctionBuilder<'a, Out, V>,
        args: I,
    ) -> Result<Self::Output>
    where
        Out: QbeFunctionOutput<'a>,
        I: IntoIterator<Item = A>,
        A: Into<QbeValue<'a>>;
}

/// Trait for variadic QBE callable objects.
pub trait QbeVariadicFunctionCall<'a> {
    type Output: QbeFunctionOutput<'a>;
    fn call_va_on<Out, I, A, const V: bool>(
        &self,
        caller: &mut QbeFunctionBuilder<'a, Out, V>,
        args: I,
        va_args: I,
    ) -> Result<Self::Output>
    where
        Out: QbeFunctionOutput<'a>,
        I: IntoIterator<Item = A>,
        A: Into<QbeValue<'a>>;
}

macro_rules! unop {
    ($life:lifetime, $name:ident, $input:ident, $valid:ident, $outtype:expr) => {
        paste! {
            #[doc = concat!("Compile the `", stringify!($name), "` QBE operation.")]
            pub fn $name<T: Into<QbeValue<$life>>>(&mut self, val: T) -> Result<QbeValue<$life>> {
                let $input = val.into();
                if !$input.type_of().[<is_ $valid>]() {
                    return Err(QbeError::IncorrectType(stringify!($valid)));
                }
                let outtyp = $outtype.promote();
                let id = self.local_counter;
                write!(&mut self.compiled,
                    concat!("\t%_{} ={} ", stringify!($name), " "),
                    id, outtyp.basic_name())?;
                $input.gen(&mut self.compiled)?;
                self.compiled.write_char('\n')?;
                self.local_counter += 1;
                Ok(QbeValue::Temporary(outtyp, id))
            }
        }
    };

    ($life:lifetime, $name:ident, $input:ident, $valid:ident, $outtype:expr, $op_name:ident) => {
        paste! {
            #[doc = concat!("Compile the `", stringify!($op_name), "` QBE operation.")]
            pub fn $name<T: Into<QbeValue<$life>>>(&mut self, val: T) -> Result<QbeValue<$life>> {
                let $input = val.into();
                if !$input.type_of().[<is_ $valid>]() {
                    return Err(QbeError::IncorrectType(stringify!($valid)));
                }
                let outtyp = $outtype.promote();
                let id = self.local_counter;
                write!(&mut self.compiled,
                    concat!("\t%_{} ={} ", stringify!($op_name), " "),
                    id, outtyp.basic_name())?;
                $input.gen(&mut self.compiled)?;
                self.compiled.write_char('\n')?;
                self.local_counter += 1;
                Ok(QbeValue::Temporary(outtyp, id))
            }
        }
    };
}

macro_rules! binop {
    ($life:lifetime, $name:ident, $in1:ident, $in2:ident, $valid:ident, $outtype:expr) => {
        paste! {
            #[doc = concat!("Compile the `", stringify!($name), "` QBE operation.")]
            pub fn $name<X, Y>(&mut self, val1: X, val2: Y) -> Result<QbeValue<$life>>
            where X: Into<QbeValue<$life>>, Y: Into<QbeValue<$life>> {
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
                write!(&mut self.compiled,
                    concat!("\t%_{} ={} ", stringify!($name), " "),
                    id, outtyp.basic_name())?;
                $in1.gen(&mut self.compiled)?;
                self.compiled.write_str(", ")?;
                $in2.gen(&mut self.compiled)?;
                self.compiled.write_char('\n')?;
                self.local_counter += 1;
                Ok(QbeValue::Temporary(outtyp, id))
            }
        }
    };

    ($life:lifetime, $name:ident, $in1:ident, $in2:ident, $valid:ident, $outtype:expr, $op_name:ident) => {
        paste! {
            #[doc = concat!("Compile the `", stringify!($op_name), "` QBE operation.")]
            pub fn $name<X, Y>(&mut self, val1: X, val2: Y) -> Result<QbeValue<$life>>
            where X: Into<QbeValue<$life>>, Y: Into<QbeValue<$life>> {
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
                write!(&mut self.compiled,
                    concat!("\t%_{} ={} ", stringify!($op_name), " "),
                    id, outtyp.basic_name())?;
                $in1.gen(&mut self.compiled)?;
                self.compiled.write_str(", ")?;
                $in2.gen(&mut self.compiled)?;
                self.compiled.write_char('\n')?;
                self.local_counter += 1;
                Ok(QbeValue::Temporary(outtyp, id))
            }
        }
    };
}

macro_rules! cmp_op {
    ($life:lifetime, $name:ident, $valid:ident) => {
        paste! {
            #[doc = concat!("Compile the `", stringify!($name), "` QBE operation.")]
            pub fn $name<X, Y>(&mut self, val1: X, val2: Y) -> Result<QbeValue<$life>>
            where X: Into<QbeValue<$life>>, Y: Into<QbeValue<$life>> {
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
                self.compiled.write_str(", ")?;
                val2.gen(&mut self.compiled)?;
                self.compiled.write_char('\n')?;
                self.local_counter += 1;
                Ok(QbeValue::Temporary(QbeType::Long, id))
            }
            #[doc = concat!("Compile the `", stringify!($name), "` QBE operation, but cast the result as a [`QbeType::Word`].")]
            pub fn [<$name _w>]<X, Y>(&mut self, val1: X, val2: Y) -> Result<QbeValue<$life>>
            where X: Into<QbeValue<$life>>, Y: Into<QbeValue<$life>> {
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
                self.compiled.write_str(", ")?;
                val2.gen(&mut self.compiled)?;
                self.compiled.write_char('\n')?;
                self.local_counter += 1;
                Ok(QbeValue::Temporary(QbeType::Word, id))
            }
        }
    };
}

#[derive(Debug)]
pub struct QbeFunctionBuilder<'a, Out: QbeFunctionOutput<'a>, const VARIADIC: bool> {
    params: &'a [QbeType],
    pub(crate) local_counter: u32,
    block_counter: u32,
    pub(crate) compiled: String,
    returned: Vec<Out::UserData>,
    names: &'a mut Vec<Pin<Box<str>>>,
}
impl<'a, Out: QbeFunctionOutput<'a>, const VARIADIC: bool> QbeFunctionBuilder<'a, Out, VARIADIC> {
    pub(crate) fn new(
        params: &'a [QbeType],
        names: &'a mut Vec<Pin<Box<str>>>,
    ) -> QbeFunctionBuilder<'a, Out, VARIADIC> {
        QbeFunctionBuilder {
            params,
            local_counter: params.len() as u32,
            // 0 is reserved for start block, 1 is reserved for end block
            block_counter: 2,
            // @_0 is the start block
            compiled: String::from("@_0\n"),
            returned: vec![],
            names,
        }
    }

    #[inline]
    pub fn start(&self) -> QbeLabel {
        QbeLabel(0)
    }

    #[inline]
    pub fn end(&self) -> QbeLabel {
        QbeLabel(1)
    }

    #[inline]
    pub fn argument(&self, idx: usize) -> Result<QbeValue<'a>> {
        if self.params.len() >= idx {
            Err(QbeError::ArgumentOutOfBounds)
        } else {
            // user-defined types are passed as pointers
            Ok(QbeValue::Temporary(
                self.params[idx].pointer_ud(),
                idx.try_into().unwrap(),
            ))
        }
    }

    #[inline]
    pub fn initialize(&mut self, typ: QbeType) -> QbeValue<'a> {
        let id = self.local_counter;
        self.local_counter += 1;
        QbeValue::Temporary(typ, id)
    }
    pub fn reassign<
        F: FnOnce(&mut QbeFunctionBuilder<'a, Out, VARIADIC>) -> Result<QbeValue<'a>>,
    >(
        &mut self,
        val: QbeValue<'a>,
        mutator: F,
    ) -> Result<()> {
        let old_counter = self.local_counter;
        let new_counter = match val {
            QbeValue::Temporary(_, id) => id,
            _ => return Err(QbeError::NonLocalRedefinition),
        };
        self.local_counter = new_counter;
        let out = mutator(self)?;
        match out {
            QbeValue::Temporary(_, id) => {
                if id != new_counter {
                    return Err(QbeError::ReassignmentSingleExpr);
                }
            }
            _ => return Err(QbeError::ReassignmentSingleExpr),
        };
        self.local_counter = old_counter;
        Ok(())
    }

    #[inline]
    pub fn global_symbol(&mut self, sym: &str) -> QbeValue<'a> {
        let s = Box::into_pin(Box::<str>::from(sym));
        self.names.push(s);
        // `transmute` to fix lifetime issue
        QbeValue::Named(unsafe { std::mem::transmute(&*self.names[self.names.len() - 1]) })
    }
    #[inline]
    pub fn thread_local_symbol(&mut self, sym: &str) -> QbeValue<'a> {
        let s = Box::into_pin(Box::<str>::from(sym));
        self.names.push(s);
        // `transmute` to fix lifetime issue
        QbeValue::ThreadLocalNamed(unsafe {
            std::mem::transmute(&*self.names[self.names.len() - 1])
        })
    }

    #[inline]
    pub fn block(&mut self) -> Result<QbeLabel> {
        let id = self.block_counter;
        writeln!(&mut self.compiled, "@_{}", id)?;
        self.block_counter += 1;
        Ok(QbeLabel(id))
    }
    #[inline]
    pub fn block_at(&mut self, at: QbeForwardLabel) -> Result<QbeLabel> {
        let id = at.0;
        writeln!(&mut self.compiled, "@_{}", id)?;
        Ok(QbeLabel(id))
    }
    #[inline]
    pub fn forward_declare_block(&mut self) -> QbeForwardLabel {
        let id = self.block_counter;
        self.block_counter += 1;
        QbeForwardLabel(id)
    }

    // phi
    pub fn phi<X, XL, Y, YL>(
        &mut self,
        path1: XL,
        val1: X,
        path2: YL,
        val2: Y,
    ) -> Result<QbeValue<'a>>
    where
        X: Into<QbeValue<'a>>,
        XL: Into<QbeLabel>,
        Y: Into<QbeValue<'a>>,
        YL: Into<QbeLabel>,
    {
        let id = self.local_counter;
        let val1 = val1.into();
        let val2 = val2.into();
        let t = val1.common_type(&val2)?;
        let t = t.promote();
        write!(
            &mut self.compiled,
            "\t%_{} ={} phi @_{} ",
            id,
            t.basic_name(),
            path1.into().0
        )?;
        val1.gen(&mut self.compiled)?;
        write!(&mut self.compiled, ", @_{} ", path2.into().0)?;
        val2.gen(&mut self.compiled)?;
        self.compiled.write_char('\n')?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t, id))
    }
    pub fn phi_t<X, XL, Y, YL>(
        &mut self,
        path1: XL,
        val1: X,
        path2: YL,
        val2: Y,
        t: QbeType,
    ) -> Result<QbeValue<'a>>
    where
        X: Into<QbeValue<'a>>,
        XL: Into<QbeLabel>,
        Y: Into<QbeValue<'a>>,
        YL: Into<QbeLabel>,
    {
        let id = self.local_counter;
        let t = t.promote();
        write!(
            &mut self.compiled,
            "\t%_{} ={} phi @_{} ",
            id,
            t.basic_name(),
            path1.into().0
        )?;
        val1.into().gen(&mut self.compiled)?;
        write!(&mut self.compiled, ", @_{} ", path2.into().0)?;
        val2.into().gen(&mut self.compiled)?;
        self.compiled.write_char('\n')?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t, id))
    }

    // call
    #[inline]
    pub fn call<F, I, A>(&mut self, func: F, args: I) -> Result<F::Output>
    where
        F: QbeFunctionCall<'a>,
        I: IntoIterator<Item = A>,
        A: Into<QbeValue<'a>>,
    {
        func.call_on(self, args)
    }
    #[inline]
    pub fn call_va<F, I, A>(&mut self, func: F, args: I, va_args: I) -> Result<F::Output>
    where
        F: QbeVariadicFunctionCall<'a>,
        I: IntoIterator<Item = A>,
        A: Into<QbeValue<'a>>,
    {
        func.call_va_on(self, args, va_args)
    }

    // arithmetic instructions
    binop! {'a, add, x, y, numeric, x.common_type(&y)?}
    binop! {'a, sub, x, y, numeric, x.common_type(&y)?}
    binop! {'a, div, x, y, numeric, x.common_type(&y)?}
    binop! {'a, mul, x, y, numeric, x.common_type(&y)?}
    unop! {'a, neg, val, numeric, val.type_of()}
    binop! {'a, udiv, x, y, integer, x.common_type(&y)?}
    binop! {'a, rem, x, y, integer, x.common_type(&y)?}
    binop! {'a, urem, x, y, integer, x.common_type(&y)?}
    binop! {'a, or, x, y, integer, x.common_type(&y)?}
    binop! {'a, xor, x, y, integer, x.common_type(&y)?}
    binop! {'a, and, x, y, integer, x.common_type(&y)?}
    binop! {'a, sar, x, y, integer, x.type_of()}
    binop! {'a, shr, x, y, integer, x.type_of()}
    binop! {'a, shl, x, y, integer, x.type_of()}

    // comparison instructions
    cmp_op! {'a, eq, any}
    cmp_op! {'a, ne, any}
    cmp_op! {'a, sle, integer}
    cmp_op! {'a, slt, integer}
    cmp_op! {'a, sge, integer}
    cmp_op! {'a, sgt, integer}
    cmp_op! {'a, ule, integer}
    cmp_op! {'a, ult, integer}
    cmp_op! {'a, uge, integer}
    cmp_op! {'a, ugt, integer}
    cmp_op! {'a, le, floating}
    cmp_op! {'a, lt, floating}
    cmp_op! {'a, ge, floating}
    cmp_op! {'a, gt, floating}
    cmp_op! {'a, o, floating}
    cmp_op! {'a, uo, floating}

    // conversion instructions
    unop! {'a, extsw, val, integer, QbeType::Long}
    unop! {'a, extuw, val, integer, QbeType::Long}
    unop! {'a, extsh, val, integer, QbeType::Long}
    unop! {'a, extuh, val, integer, QbeType::Long}
    unop! {'a, extsb, val, integer, QbeType::Long}
    unop! {'a, extub, val, integer, QbeType::Long}
    unop! {'a, extsh_w, val, integer, QbeType::Word, extsh}
    unop! {'a, extuh_w, val, integer, QbeType::Word, extuh}
    unop! {'a, extsb_w, val, integer, QbeType::Word, extsb}
    unop! {'a, extub_w, val, integer, QbeType::Word, extub}
    unop! {'a, exts, val, floating, QbeType::Double}
    unop! {'a, truncd, val, floating, QbeType::Single}
    unop! {'a, stosi, val, single, QbeType::Long}
    unop! {'a, stoui, val, single, QbeType::Long}
    unop! {'a, dtosi, val, double, QbeType::Long}
    unop! {'a, dtoui, val, double, QbeType::Long}
    unop! {'a, stosi_w, val, single, QbeType::Word, stosi}
    unop! {'a, stoui_w, val, single, QbeType::Word, stoui}
    unop! {'a, dtosi_w, val, double, QbeType::Word, dtosi}
    unop! {'a, dtoui_w, val, double, QbeType::Word, dtoui}
    unop! {'a, swtof, val, word, QbeType::Double}
    unop! {'a, uwtof, val, word, QbeType::Double}
    unop! {'a, sltof, val, long, QbeType::Double}
    unop! {'a, ultof, val, long, QbeType::Double}
    unop! {'a, swtof_s, val, word, QbeType::Single, swtof}
    unop! {'a, uwtof_s, val, word, QbeType::Single, uwtof}
    unop! {'a, sltof_s, val, long, QbeType::Single, sltof}
    unop! {'a, ultof_s, val, long, QbeType::Single, ultof}

    // copy and cast
    unop! {'a, copy, val, any, val.type_of()}
    unop! {'a, cast, val, numeric, val.type_of().promote().cast()}

    // memory instructions
    pub fn store<X, Y>(&mut self, val: X, to: Y) -> Result<()>
    where
        X: Into<QbeValue<'a>>,
        Y: Into<QbeValue<'a>>,
    {
        let to = to.into();
        if !to.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        let val = val.into();
        let typ = match val.type_of() {
            QbeType::Word => "w",
            QbeType::Long => "l",
            QbeType::Single => "s",
            QbeType::Double => "d",
            QbeType::Byte => "b",
            QbeType::Half => "h",
            QbeType::SignedByte => "b",
            QbeType::SignedHalf => "h",
            QbeType::UserDefined(_) => "l",
        };
        write!(&mut self.compiled, "\tstore{} ", typ)?;
        val.gen(&mut self.compiled)?;
        self.compiled.write_str(", ")?;
        to.gen(&mut self.compiled)?;
        self.compiled.write_char('\n')?;
        Ok(())
    }
    pub fn store_t<X, Y>(&mut self, val: X, to: Y, t: QbeType) -> Result<()>
    where
        X: Into<QbeValue<'a>>,
        Y: Into<QbeValue<'a>>,
    {
        let to = to.into();
        if !to.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        let val = val.into();
        write!(&mut self.compiled, "\tstore{} ", t.basic_name())?;
        val.gen(&mut self.compiled)?;
        self.compiled.write_str(", ")?;
        to.gen(&mut self.compiled)?;
        self.compiled.write_char('\n')?;
        Ok(())
    }
    pub fn load<T: Into<QbeValue<'a>>>(&mut self, from: T, t: QbeType) -> Result<QbeValue<'a>> {
        let id = self.local_counter;
        let from = from.into();
        if !from.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        match t.basic_name() {
            "w" => write!(&mut self.compiled, "\t%_{} =w loaduw ", id)?,
            "l" => write!(&mut self.compiled, "\t%_{} =l loadl ", id)?,
            "s" => write!(&mut self.compiled, "\t%_{} =s loads ", id)?,
            "d" => write!(&mut self.compiled, "\t%_{} =d loadd ", id)?,
            "b" => write!(&mut self.compiled, "\t%_{} =w loadub ", id)?,
            "h" => write!(&mut self.compiled, "\t%_{} =w loaduh ", id)?,
            _ => unreachable!(),
        };
        from.gen(&mut self.compiled)?;
        self.compiled.write_char('\n')?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t.promote(), id))
    }
    pub fn load_signed<T: Into<QbeValue<'a>>>(
        &mut self,
        from: T,
        t: QbeType,
    ) -> Result<QbeValue<'a>> {
        let id = self.local_counter;
        let from = from.into();
        if !from.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        match t.basic_name() {
            "w" => write!(&mut self.compiled, "\t%_{} =w loadsw ", id)?,
            "l" => write!(&mut self.compiled, "\t%_{} =l loadl ", id)?,
            "s" => write!(&mut self.compiled, "\t%_{} =s loads ", id)?,
            "d" => write!(&mut self.compiled, "\t%_{} =d loadd ", id)?,
            "b" => write!(&mut self.compiled, "\t%_{} =w loadsb ", id)?,
            "h" => write!(&mut self.compiled, "\t%_{} =w loadsh ", id)?,
            _ => unreachable!(),
        };
        from.gen(&mut self.compiled)?;
        self.compiled.write_char('\n')?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t.promote(), id))
    }
    pub fn blit<X, Y, Z>(&mut self, src: X, dst: Y, size: Z) -> Result<()>
    where
        X: Into<QbeValue<'a>>,
        Y: Into<QbeValue<'a>>,
        Z: Into<QbeValue<'a>>,
    {
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
        self.compiled.write_str("\tblit ")?;
        src.gen(&mut self.compiled)?;
        self.compiled.write_str(", ")?;
        dst.gen(&mut self.compiled)?;
        self.compiled.write_str(", ")?;
        size.gen(&mut self.compiled)?;
        self.compiled.write_char('\n')?;
        Ok(())
    }
    unop! {'a, alloc4, val, integer, QbeType::Long}
    unop! {'a, alloc8, val, integer, QbeType::Long}
    unop! {'a, alloc16, val, integer, QbeType::Long}

    // jumps
    #[inline]
    pub fn jmp<T: Into<QbeLabel>>(&mut self, to: T) -> Result<()> {
        writeln!(&mut self.compiled, "\tjmp @_{}", to.into().0)?;
        Ok(())
    }
    pub fn jnz<T, YL, NL>(&mut self, cond: T, yes: YL, no: NL) -> Result<()>
    where
        T: Into<QbeValue<'a>>,
        YL: Into<QbeLabel>,
        NL: Into<QbeLabel>,
    {
        self.compiled.write_str("\tjnz ")?;
        cond.into().gen(&mut self.compiled)?;
        writeln!(
            &mut self.compiled,
            ", @_{}, @_{}",
            yes.into().0,
            no.into().0
        )?;
        Ok(())
    }
    #[inline]
    pub fn hlt(&mut self) -> Result<()> {
        self.compiled.write_str("\thlt\n")?;
        Ok(())
    }

    #[inline]
    pub fn early_return<T: Into<Out>>(&mut self, val: T) -> Result<()> {
        let val = val.into();
        val.func_return(self)?;
        self.returned.push(val.get_ud());
        Ok(())
    }

    pub(crate) fn build<F>(&mut self, builder: F) -> Result<Out>
    where
        F: FnOnce(&mut QbeFunctionBuilder<'a, Out, VARIADIC>) -> Result<Out>,
    {
        let ret = builder(self)?;
        let associated = ret.get_ud();

        for i in self.returned.iter() {
            if i != &associated {
                return Err(QbeError::DisagreeingReturns);
            }
        }

        // label the end block in case anything uses it as a destination
        self.compiled.write_str("@_1\n")?;
        ret.func_return(self)?;
        self.compiled.write_char('\n')?;
        Ok(ret)
    }
}

impl<'a, Out: QbeFunctionOutput<'a>> QbeFunctionBuilder<'a, Out, VARIADIC_FUNC> {
    // variadic arguments
    pub fn vastart<T: Into<QbeValue<'a>>>(&mut self, at: T) -> Result<()> {
        let at = at.into();
        if !at.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        self.compiled.write_str("\tvastart ")?;
        at.gen(&mut self.compiled)?;
        self.compiled.write_char('\n')?;
        Ok(())
    }
    pub fn vaarg<T: Into<QbeValue<'a>>>(&mut self, from: T, t: QbeType) -> Result<QbeValue<'a>> {
        let from = from.into();
        if !from.type_of().is_pointer() {
            return Err(QbeError::IncorrectType("pointer"));
        }
        let id = self.local_counter;
        let t = t.promote();
        write!(&mut self.compiled, "\t%_{} ={} vaarg ", id, t.basic_name())?;
        from.gen(&mut self.compiled)?;
        self.local_counter += 1;
        Ok(QbeValue::Temporary(t, id))
    }
}
