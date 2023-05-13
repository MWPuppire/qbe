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

#[cfg(feature = "qbe-command")]
pub enum QbeTarget {
    Amd64,
    Amd64Apple,
    Arm64,
    Arm64Apple,
    RiscV64,
}
#[cfg(feature = "qbe-command")]
impl QbeTarget {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Amd64 => "amd64_sysv",
            Self::Amd64Apple => "amd64_apple",
            Self::Arm64 => "arm64",
            Self::Arm64Apple => "arm64_apple",
            Self::RiscV64 => "rv64",
        }
    }
}
