
#[derive(PartialEq, Eq, Hash)]
pub enum Operations{
    Unary(UnaryOperations),
    Binary(BinaryOperations),
}
pub const OP_TYPE_AMOUNT:usize = UNARY_OP_TYPE_AMOUNT + BINARY_OP_TYPE_AMOUNT;

#[derive(PartialEq, Eq, Hash)]
pub enum UnaryOperations{ Not, }
pub const UNARY_OP_TYPE_AMOUNT:usize = 1;

#[derive(PartialEq, Eq, Hash)]
pub enum BinaryOperations{
    And,
    Or,
    Impl,
}
pub const BINARY_OP_TYPE_AMOUNT:usize = 3;
