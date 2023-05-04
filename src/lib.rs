#[macro_use]
extern crate derive_builder;
#[macro_use]
extern crate thiserror;

mod value;
mod func;
mod context;

pub use value::*;
pub use func::*;
pub use context::{QbeContext, QbeDeclBuilder};

#[derive(Error, Debug)]
pub enum QbeError {
    #[error("unknown error while compiling")]
    CompileError(#[from] std::fmt::Error),
    #[error("expected return type {0}")]
    IncorrectReturn(QbeType),
    #[error("expected {0} parameter")]
    IncorrectType(&'static str),
    #[error("cannot return a value from this function")]
    CannotReturnValue,
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
    #[error("no return found in a function with a return type")]
    NoReturn,
    #[error("cannot use a user-defined type in that context")]
    NotBasic,
}

pub type Result<T> = std::result::Result<T, QbeError>;
