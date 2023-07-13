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

pub type Result<T> = std::result::Result<T, QbeError>;
