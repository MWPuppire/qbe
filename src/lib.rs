//! A binding and wrapper for [QBE](https://c9x.me/compile) in Rust. Provides a
//! class to create QBE IR easily and compile it to assembly when finished.

#[macro_use]
extern crate thiserror;
extern crate cfg_if;
#[cfg(feature = "qbe-compile")]
extern crate errno;
#[cfg(feature = "qbe-compile")]
extern crate libc;
extern crate paste;

mod context;
mod func;
#[cfg(feature = "qbe-compile")]
mod qbe_wrapper;
mod value;

pub use context::{QbeContext, QbeDecl};
pub use func::*;
pub use value::*;

#[cfg(feature = "qbe-compile")]
use qbe_wrapper::{write_assembly_to_file, write_assembly_to_string, CFile};

/// The type for errors which may occur while building QBE programs.
#[derive(Debug, Error)]
pub enum QbeError {
    /// Wrapper over [`std::fmt::Error`], which may result when generating QBE
    /// code from expressions.
    #[error("unknown error while compiling")]
    CompileError(#[from] std::fmt::Error),
    /// Error used when a function was provided a value of an incorrect type;
    /// expected types are provided in the parameter.
    #[error("expected {0} parameter")]
    IncorrectType(&'static str),
    /// Error used when a function requests an argument beyond those which were
    /// specified in it.
    #[error("argument out of bounds")]
    ArgumentOutOfBounds,
    /// Error used when a forward declaration, upon initializes, specifies an
    /// export name.
    #[error("cannot use `export_as` with a forward declaration")]
    ForwardDeclareName,
    /// Error used when an operation is used between two types that have no
    /// common type that can be found. Either one of the types should be
    /// changed, or a different operation that allows specifying an output type
    /// should be used.
    #[error("cannot infer a common type")]
    CannotInferType,
    /// Error used when [`reassign`][QbeFunctionBuilder::reassign] is used with
    /// a non-local variable.
    #[error("cannot reassign to anything other than a local variable")]
    NonLocalRedefinition,
    /// Error used when [`reassign`][QbeFunctionBuilder::reassign] is given an
    /// unexpected function body. [`reassign`][QbeFunctionBuilder::reassign]
    /// should only be used with a single expression returning a local value;
    /// the code has no way to guarantee only a single expression is used, but a
    /// non-local value being returned results in this error being raised.
    #[error("reassignment must use only a single expression returning a local")]
    ReassignmentSingleExpr,
    /// Error used when a user-defined type is provided into a context that
    /// expects only a basic primitive type. Often, this means that the user-
    /// defined type should be passed in as a pointer instead of plainly.
    #[error("cannot use a user-defined type in that context")]
    NotBasic,
    /// Error used when a function has multiple return values (e.g., from
    /// [`early_return`][QbeFunctionBuilder::early_return]) and the returned
    /// values have different types.
    #[error("returns in a function disagree on return type")]
    DisagreeingReturns,
}

/// Convenience wrapper over `Result` to avoid re-writing `QbeError` everywhere.
pub type Result<T> = std::result::Result<T, QbeError>;

/// An enumeration of QBE target architectures and platforms. These can be
/// passed to functions that compile QBE IR to assembly to specify what assembly
/// should be generated. `Default` is implemented for `QbeTarget` if the target
/// is one that QBE supports; otherwise, `Default` isn't implemented and the
/// short-hand functions that compile to the default architecture aren't
/// provided.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum QbeTarget {
    Amd64,
    Amd64Apple,
    Arm64,
    Arm64Apple,
    RiscV64,
}
#[cfg(all(
    not(windows),
    any(
        target_arch = "x86_64",
        target_arch = "aarch64",
        target_arch = "riscv64gc"
    )
))]
impl Default for QbeTarget {
    #[inline]
    fn default() -> Self {
        cfg_if::cfg_if! {
            if #[cfg(all(target_arch = "x86_64", any(target_os = "macos", target_os = "ios")))] {
                Self::Amd64Apple
            } else if #[cfg(target_arch = "x86_64")] {
                Self::Amd64Sysv
            } else if #[cfg(all(target_arch = "aarch64", any(target_os = "macos", target_os = "ios")))] {
                Self::Arm64Apple
            } else if #[cfg(target_arch = "aarch64")] {
                Self::Arm64
            } else if #[cfg(target_arch = "riscv64gc")] {
                Self::RiscV64
            }
        }
    }
}

/// Compile QBE IR from a string to assembly for the current platform. Short for
/// [`ir_to_target_assembly(ir, QbeTarget::default())`][ir_to_target_assembly].
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
pub fn ir_to_assembly(ir: &str) -> std::result::Result<String, errno::Errno> {
    write_assembly_to_string(ir, QbeTarget::default())
}
/// Compile QBE IR from a string to assembly for `target`. Note that, due to how
/// QBE works, this function involves writing to and reading from a temporary
/// file; if your end goal is to have assembly in a file rather than a string,
/// [`ir_to_target_assembly_file`] will most likely be faster.
#[cfg(feature = "qbe-compile")]
#[inline]
pub fn ir_to_target_assembly(
    ir: &str,
    target: QbeTarget,
) -> std::result::Result<String, errno::Errno> {
    write_assembly_to_string(ir, target)
}
/// Compile QBE IR from a string and write it to assembly file `file_name`.
/// Equivalent to [`ir_to_target_assembly_file(ir, file_name, QbeTarget::default())`][ir_to_target_assembly_file].
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
pub fn ir_to_assembly_file(ir: &str, file_name: &str) -> std::result::Result<(), errno::Errno> {
    let f = CFile::open(file_name, b"w\0")?;
    write_assembly_to_file(ir, QbeTarget::default(), &f)
}
/// Compile QBE IR from a string and write it to assembly file `file_name` for
/// `target`.
#[cfg(feature = "qbe-compile")]
#[inline]
pub fn ir_to_target_assembly_file(
    ir: &str,
    file_name: &str,
    target: QbeTarget,
) -> std::result::Result<(), errno::Errno> {
    let f = CFile::open(file_name, b"w\0")?;
    write_assembly_to_file(ir, target, &f)
}
