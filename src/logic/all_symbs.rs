use super::{
    syntax_symbs::{SYNTAX_SYMBS_AMOUNT, SyntaxSymbs}, 
    operations::{OP_TYPE_AMOUNT, Operations}, 
    term_type::{TERM_TYPE_AMOUNT, TermType},
    quants::{QUANTS_TYPE_AMOUNT, Quants}, 
};


#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum AllSymbs{
    Op(Operations),
    Quant(Quants),
    Term(TermType),

    Syntax(SyntaxSymbs),

    Empty, // Space and so on
    //End, // only in end of 
}
pub const SYMBS_AMOUNT:usize = 1 + SYNTAX_SYMBS_AMOUNT + OP_TYPE_AMOUNT + QUANTS_TYPE_AMOUNT + TERM_TYPE_AMOUNT;

impl AllSymbs{
    pub fn is_empty(&self) -> bool { AllSymbs::Empty == *self }
}