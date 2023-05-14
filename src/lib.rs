#[macro_use]
extern crate thiserror;

#[macro_use]
extern crate derive_builder;

extern crate cfg_if;

mod value;
mod func;
mod context;
mod qbe_wrapper;

pub use value::*;
pub use func::*;
pub use context::{QbeContext, QbeDeclBuilder};

#[derive(Debug)]
#[derive(Error)]
pub enum QbeError {
    #[error("unknown error while compiling")]
    CompileError(#[from] std::fmt::Error),
    #[error("expected {0} parameter")]
    IncorrectType(&'static str),
    #[error("argument out of bounds")]
    ArgumentOutOfBounds,
    #[error("this function doesn't have an env argument")]
    NoEnvArgument,
    #[error("cannot use `export_as` with a forward declaration")]
    ForwardDeclareName,
    #[error("cannot infer a common type")]
    CannotInferType,
    #[error("cannot call anything other than a global symbol")]
    NonGlobalCall,
    #[error("cannot use `vastart` and `vaarg` outside a variadic function")]
    NonVariadic,
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
