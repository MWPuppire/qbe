#[macro_use]
extern crate derive_builder;
#[macro_use]
extern crate thiserror;

mod value;
mod func;
mod context;

pub use value::*;
pub use func::*;
pub use context::QbeContext;

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
    #[error("cannot redefine a value with `function_at` or `global_at`")]
    AlreadyDefined,
    #[error("cannot use `export_as` with a forward declaration")]
    ForwardDeclareName,
    #[error("cannot infer a common type")]
    CannotInferType,
    #[error("cannot call anything other than a global symbol")]
    NonGlobalCall,
    #[error("cannot use `vastart` and `vaarg` outside a variadic function")]
    NonVariadic,
}

pub type Result<T> = std::result::Result<T, QbeError>;
