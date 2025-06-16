//! An op is either an instruction or a variable definition.

use dsa::common::prelude::*;
use std::sync::Arc;

// /// An instruction or variable definition.
// pub enum Op {
//     Instruction(InstructionOp),
//     VariableDefinition(VariableDefinitionOp),
// }

pub enum Node {
    Register(Register),
    /// Symbol name to be resolved to an address.
    Symbol(Arc<str>),
    /// Literal values **within instructions** are always `u16`, but variables
    /// may be larger.
    Literal(u32),
    Instruction,
}
