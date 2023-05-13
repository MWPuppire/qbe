#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
extern crate std;

#[cfg(feature = "thiserror")]
#[macro_use]
extern crate thiserror;

#[macro_use]
extern crate derive_builder;

extern crate cfg_if;

mod value;
mod func;
mod context;

pub use value::*;
pub use func::*;
pub use context::{QbeContext, QbeDeclBuilder};

#[derive(Debug)]
#[cfg_attr(feature = "thiserror", derive(Error))]
pub enum QbeError {
    #[cfg_attr(feature = "thiserror", error("unknown error while compiling"))]
    CompileError(#[cfg_attr(feature = "thiserror", from)] core::fmt::Error),
    #[cfg_attr(feature = "thiserror", error("expected {0} parameter"))]
    IncorrectType(&'static str),
    #[cfg_attr(feature = "thiserror", error("argument out of bounds"))]
    ArgumentOutOfBounds,
    #[cfg_attr(feature = "thiserror", error("this function doesn't have an env argument"))]
    NoEnvArgument,
    #[cfg_attr(feature = "thiserror", error("cannot use `export_as` with a forward declaration"))]
    ForwardDeclareName,
    #[cfg_attr(feature = "thiserror", error("cannot infer a common type"))]
    CannotInferType,
    #[cfg_attr(feature = "thiserror", error("cannot call anything other than a global symbol"))]
    NonGlobalCall,
    #[cfg_attr(feature = "thiserror", error("cannot use `vastart` and `vaarg` outside a variadic function"))]
    NonVariadic,
    #[cfg_attr(feature = "thiserror", error("cannot reassign to anything other than a local variable"))]
    NonLocalRedefinition,
    #[cfg_attr(feature = "thiserror", error("reassignment must use only a single expression returning a local"))]
    ReassignmentSingleExpr,
    #[cfg_attr(feature = "thiserror", error("cannot use a user-defined type in that context"))]
    NotBasic,
    #[cfg_attr(feature = "thiserror", error("returns in a function disagree on return type"))]
    DisagreeingReturns,
}
#[cfg(not(feature = "thiserror"))]
impl From<core::fmt::Error> for QbeError {
    fn from(item: core::fmt::Error) -> Self {
        QbeError::CompileError(item)
    }
}

pub type Result<T> = core::result::Result<T, QbeError>;

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
