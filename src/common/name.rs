use std::{hash::Hash, fmt};

use crate::logic::term_type::TermType;

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct StdName{
    pub name_type: TermType,
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


impl Name for StdName {
    fn first_name(term_type: TermType) -> Self { Self { name_type: term_type, name: 0, index: 0 } }

    fn name_type(&self) -> TermType { self.name_type }
    fn next_tst_name(&self) -> Self { Self { name_type: self.name_type, name: self.name + 1, index: self.index } }
    fn next_tst_index(&self) -> Self { Self { name_type: self.name_type, name: self.name, index: self.index + 1 } }
}

pub trait Name where Self: Eq + Clone + Hash{
    
    fn first_name(term_type: TermType) -> Self;

    fn name_type(&self) -> TermType;
    /// tst mean the same type
    fn next_tst_name(&self) -> Self;
    /// tst mean the same type
    fn next_tst_index(&self) -> Self;
    /*
    fn new(name_type: NameType) -> Self;
    */
}



impl fmt::Display for StdName{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { 
        match self.name_type {
            TermType::Const => write!(f, "c")?, 
            TermType::Var => write!(f, "X")?, 
            TermType::Func => write!(f, "f")?, 
            TermType::Pred => write!(f, "P")?, 
        }
        write!(f, "[{}][i:{}]", self.name, self.index)
    }
}
