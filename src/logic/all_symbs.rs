use super::{
    operations::{OP_TYPE_AMOUNT, Operations}, 
    term_type::{TERM_TYPE_AMOUNT, TermType},
    quants::{QUANTS_TYPE_AMOUNT, Quants}
};


#[derive(PartialEq, Eq, Hash)]
pub enum AllSymbs{
    Op(Operations),
    Quant(Quants),
    Term(TermType),
}
pub const SYMBS_AMOUNT:usize = OP_TYPE_AMOUNT + QUANTS_TYPE_AMOUNT + TERM_TYPE_AMOUNT;