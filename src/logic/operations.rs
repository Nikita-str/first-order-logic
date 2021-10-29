
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum Operations{
    Unary(UnaryOperations),
    Binary(BinaryOperations),
}
pub const OP_TYPE_AMOUNT:usize = UNARY_OP_TYPE_AMOUNT + BINARY_OP_TYPE_AMOUNT;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum UnaryOperations{ Not, }
pub const UNARY_OP_TYPE_AMOUNT:usize = 1;
impl UnaryOperations {
    pub fn is_not(&self) -> bool{ match self { UnaryOperations::Not => true } }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum BinaryOperations{
    And,
    Or,
    Impl,
}
pub const BINARY_OP_TYPE_AMOUNT:usize = 3;
impl BinaryOperations {
    pub fn after_logical_not(&self) -> Self{ 
        match self {
            BinaryOperations::And => BinaryOperations::Or, 
            BinaryOperations::Or => BinaryOperations::And, 
            BinaryOperations::Impl => panic!("we cant just log-not Impl operation") 
        } 
    }
}


