

pub struct ParseStr<'a>{ 
    s: &'a str 
}

impl<'a> ParseStr<'a>{
    pub fn new(s: &'a str) -> Self { Self{ s } }
    fn get_len_while<F>(&self, f: F) -> usize
    where F: Fn(char) -> bool{
        let mut sh = 0;
        self.s.chars().any(|x| if f(x) { sh = sh + x.len_utf8(); false } else { true });
        sh
    }
}

const ONE_CHARS: &'static str = "(,)∀∃¬∨∧→";
const MULTI_CHARS: &'static str = "|&->!";

impl<'a> Iterator for ParseStr<'a>{
    type Item = Self;
    fn next(&mut self) -> Option<Self::Item>{
        let c = self.s.chars().next();

        // TODO:LAZY: [only if write normal ~full parser]
        // may create ~ruleset for check next term(token-splitter sounds well)
        //          but i'm lazy to do this (and add it as ref into ParserStr)
        match c {
            None => None,
            Some(c) => {
                let sh = if c.is_whitespace() {
                    self.get_len_while(|x|x.is_whitespace())
                } else if ONE_CHARS.contains(c) {
                    c.len_utf8()
                } else if c.is_ascii_digit() {
                    self.get_len_while(|x|x.is_ascii_digit())
                } else if c.is_ascii_alphabetic() {
                    self.get_len_while(|x|x.is_ascii_alphanumeric() || x == '_')
                } else if MULTI_CHARS.contains(c){
                    self.get_len_while(|x|MULTI_CHARS.contains(x))
                } else {
                    self.get_len_while(|x|!(
                        x.is_whitespace() || 
                        x.is_ascii_alphanumeric() ||
                        MULTI_CHARS.contains(x) || 
                        ONE_CHARS.contains(x) || 
                        x == '_'
                    ))
                };
                
                let left = self.s.get(0..sh).unwrap();
                let right = self.s.get(sh..).unwrap();

                *self = Self{ s: right};
                Some(Self { s: left })
            }
        }
    }
}


#[cfg(test)]
mod parse_str_test{
    use super::ParseStr;

    fn test_help(ps: ParseStr, expects: Vec<&str>){
        let mut iter = ps.into_iter(); 
        for expect in expects{
            assert_eq!(iter.next().unwrap().s, expect)
        }
        assert!(iter.next().is_none()) 
    }

    #[test]
    fn tests(){
        test_help(
            ParseStr::new("∃x∀y P(x,y)   →∀yi∃x    P(x,  yi)"), 
        vec!["∃", "x", "∀", "y", " ", "P", "(", "x", ",", "y", ")", "   ", "→", "∀", "yi", "∃", "x", "    ", "P", "(", "x", ",", "  ", "yi", ")"]
        );

        test_help(
            ParseStr::new("∃∃ ||| &&& ---> wow_so_cool_name_101 25x25 (())"),
            vec!["∃", "∃", " ", "|||", " ", "&&&", " ", "--->", " ", "wow_so_cool_name_101", " ", "25", "x25", " ", "(", "(", ")", ")"],
        );
    }
}