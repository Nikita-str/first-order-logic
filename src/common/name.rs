use std::{hash::Hash, fmt};

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum NameType{
    Const,
    Var,
    Func,
    Pred,
}


#[derive(PartialEq, Eq, Hash, Clone)]
pub struct StdName{
    pub name_type: NameType,
    pub name: usize,
    pub index: usize,
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


impl Name for StdName {}

pub trait Name where Self: Eq + Clone + Hash{
    /*
    fn name_type(&self) -> NameType;
    fn next_index(&self) -> Self;
    fn new(name_type: NameType) -> Self;
    */
}



impl fmt::Display for StdName{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { 
        match self.name_type {
            NameType::Const => write!(f, "c")?, 
            NameType::Var => write!(f, "X")?, 
            NameType::Func => write!(f, "f")?, 
            NameType::Pred => write!(f, "P")?, 
        }
        write!(f, "[{}][i:{}]", self.name, self.index)
    }
}
