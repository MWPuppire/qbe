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
    #[error("unknown error while compiling")]
    CompileError(#[from] std::fmt::Error),
    #[error("expected {0} parameter")]
    IncorrectType(&'static str),
    #[error("argument out of bounds")]
    ArgumentOutOfBounds,
    #[error("cannot use `export_as` with a forward declaration")]
    ForwardDeclareName,
    #[error("cannot infer a common type")]
    CannotInferType,
    #[error("cannot reassign to anything other than a local variable")]
    NonLocalRedefinition,
    #[error("reassignment must use only a single expression returning a local")]
    ReassignmentSingleExpr,
    #[error("cannot use a user-defined type in that context")]
    NotBasic,
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
/// `ir_to_target_assembly(ir, QbeTarget::default())`.
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
/// Compile QBE IR from a string to assembly for `target`. Note that this
/// function, due to how QBE's API works, involves writing to a temporary file;
/// if your end goal is to have assembly in a file, `ir_to_target_assembly_file`
/// will most likely be faster.
#[cfg(feature = "qbe-compile")]
#[inline]
pub fn ir_to_target_assembly(
    ir: &str,
    target: QbeTarget,
) -> std::result::Result<String, errno::Errno> {
    write_assembly_to_string(ir, target)
}
/// Compile QBE IR from a string and write it to assembly file `file_name`.
/// Short for `ir_to_target_assembly_file(ir, file_name, QbeTarget::default())`.
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
