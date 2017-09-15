mod ast_utils;
mod check_class;
mod check_expr;
mod class_utils;
mod ctx;
pub mod expected; //TODO: not pub?
mod instantiator;
mod type_utils;

pub use self::check_class::check_module;
