//! A binding and wrapper for [QBE](https://c9x.me/compile) in Rust. Provides a
//! class to create QBE IR easily and compile it to assembly when finished.

#[macro_use]
extern crate thiserror;
#[macro_use]
extern crate derive_builder;
extern crate cfg_if;
extern crate errno;
extern crate libc;
extern crate paste;

mod context;
mod func;
mod qbe_wrapper;
mod value;

pub use context::{QbeContext, QbeDeclBuilder};
pub use func::*;
pub use qbe_wrapper::QbeTarget;
pub use value::*;

use qbe_wrapper::{CFile, write_assembly_to_file, write_assembly_to_string};

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

/// Compile QBE IR from a string to assembly for the current platform. Short for
/// `ir_to_target_assembly(ir, QbeTarget::default())`.
#[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64", target_arch = "riscv64gc")))]
#[inline]
pub fn ir_to_assembly(ir: &str) -> std::result::Result<String, errno::Errno> {
    write_assembly_to_string(ir, QbeTarget::default())
}
/// Compile QBE IR from a string to assembly for `target`. Note that this
/// function, due to how QBE's API works, involves writing to a temporary file;
/// if your end goal is to have assembly in a file, `ir_to_target_assembly_file`
/// will most likely be faster.
#[inline]
pub fn ir_to_target_assembly(ir: &str, target: QbeTarget) -> std::result::Result<String, errno::Errno> {
    write_assembly_to_string(ir, target)
}
/// Compile QBE IR from a string and write it to assembly file `file_name`.
/// Short for `ir_to_target_assembly_file(ir, file_name, QbeTarget::default())`.
#[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64", target_arch = "riscv64gc")))]
#[inline]
pub fn ir_to_assembly_file(ir: &str, file_name: &str) -> std::result::Result<(), errno::Errno> {
    let f = CFile::open(file_name, b"w\0")?;
    write_assembly_to_file(ir, QbeTarget::default(), &f)
}
/// Compile QBE IR from a string and write it to assembly file `file_name` for
/// `target`.
#[inline]
pub fn ir_to_target_assembly_file(ir: &str, file_name: &str, target: QbeTarget) -> std::result::Result<(), errno::Errno> {
    let f = CFile::open(file_name, b"w\0")?;
    write_assembly_to_file(ir, target, &f)
}
