use super::term_type::TermType;
pub trait DefaultTerm{ fn default_term(term_type: TermType) -> Self; }
