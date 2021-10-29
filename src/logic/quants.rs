
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum Quants{
    All,
    Exist,
}
pub const QUANTS_TYPE_AMOUNT:usize = 2;

impl Quants{
    pub fn not(&self) -> Self{
        match self {
            Quants::All => Quants::Exist,
            Quants::Exist => Quants::All,
        }
    }
}