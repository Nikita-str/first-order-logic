use std::hash::Hash;

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum NameType{
    Const,
    Var,
    Func,
    Pred,
}


#[derive(PartialEq, Eq)]
pub struct StdName{
    name_type: NameType,
    name: usize,
    index: usize,
}

impl StdName{
    pub fn new_next_index(&self) -> Self{
        Self{
            name_type: self.name_type,
            name: self.name,
            index: self.index + 1,
        }
    }

}



pub trait Name where Self: Eq + Clone + Hash{
    fn name_type(&self) -> NameType;
    fn next_index(&self) -> Self;
    fn new(name_type: NameType) -> Self;
}