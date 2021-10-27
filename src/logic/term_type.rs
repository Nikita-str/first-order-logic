
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum TermType{
    Const,
    Var,
    Func,
    Pred,
}
pub const TERM_TYPE_AMOUNT:usize = 4;
