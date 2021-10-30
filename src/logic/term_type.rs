
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug, Ord)]
pub enum TermType{
    Const,
    Var,
    Func,
    Pred,
}
pub const TERM_TYPE_AMOUNT:usize = 4;


#[inline]
fn into_num(tt: &TermType) -> i32 {
    match tt {
        TermType::Var => 0,
        TermType::Const => 1,
        TermType::Func => 2,
        TermType::Pred => 3,
    }
}
impl PartialOrd for TermType{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        into_num(self).partial_cmp(&into_num(other))
    }
}
