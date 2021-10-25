
pub enum Operations{
    Unary(UnaryOperations),
    Binary(BinaryOperations),
}

pub enum UnaryOperations{ Not, }

pub enum BinaryOperations{
    And,
    Or,
    Impl,
}