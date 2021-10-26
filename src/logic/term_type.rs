
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum TermType{
    Const,
    Var,
    Func,
    Pred,
}
pub const TERM_TYPE_AMOUNT:usize = 4;
