use std::{iter::Peekable, str::Chars};

#[derive(Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    Arrow,  // ->
    Lambda, // \ or λ
    Equal,
    Ident(String),
    Number(f64),
    // Bool(bool),
    Let,
    In,
}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().peekable(),
        }
    }
    pub fn tokenize(mut self) -> Vec<Token> {
        let mut tokens = vec![];

        while let Some(ch) = self.chars.next() {
            match ch {
                ' ' | '\t' | '\n' | '\r' => continue,
                '(' => tokens.push(Token::LParen),
                ')' => tokens.push(Token::RParen),
                '\\' | 'λ' => tokens.push(Token::Lambda),
                '-' => {
                    if !self.next_match('>') {
                        panic!("expected '>' after '-' to form arrow");
                    }
                    self.chars.next();
                    tokens.push(Token::Arrow);
                }
                '=' => tokens.push(Token::Equal),
                _ if ch.is_ascii_digit() => {
                    let mut n = String::new();
                    n.push(ch);
                    while let Some(next_ch) = self.chars.peek() {
                        if !next_ch.is_ascii_digit() {
                            break;
                        }
                        n.push(*next_ch);
                        self.chars.next();
                    }
                    tokens.push(Token::Number(n.parse().expect("expected valid number")));
                }
                _ if ch.is_alphabetic() => {
                    let mut ident = String::new();
                    ident.push(ch);
                    // clumsy
                    match (ch, self.chars.peek()) {
                        ('i', Some('n')) => {
                            self.chars.next();
                            tokens.push(Token::In);
                            continue;
                        }
                        ('l', Some('e')) => {
                            ident.push('e');
                            self.chars.next();
                            if self.next_match('t') {
                                self.chars.next();
                                tokens.push(Token::Let);
                                continue;
                            }
                        }
                        _ => {}
                    }
                    while let Some(next_ch) = self.chars.peek() {
                        // TODO: uhm, should be better
                        if next_ch.is_whitespace() || next_ch.is_ascii_punctuation() {
                            break;
                        }
                        ident.push(*next_ch);
                        self.chars.next();
                    }
                    tokens.push(Token::Ident(ident));
                }
                _ => panic!("unexpected char {ch}"),
            }
        }

        tokens
    }
    fn next_match(&mut self, ch: char) -> bool {
        matches!(self.chars.peek(), Some(next_ch) if *next_ch == ch)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize(src: &str) -> Vec<Token> {
        Lexer::new(src).tokenize()
    }

    #[test]
    fn test_tokenize() {
        use Token::*;
        assert_eq!(tokenize("3"), vec![Number(3.0),]);
        assert_eq!(
            tokenize("(\\x -> x) 3"),
            vec![
                LParen,
                Lambda,
                Ident("x".into()),
                Arrow,
                Ident("x".into()),
                RParen,
                Number(3.0)
            ]
        );
        assert_eq!(
            tokenize("(λx -> (λy -> x)) 1 2"),
            vec![
                LParen,
                Lambda,
                Ident("x".into()),
                Arrow,
                LParen,
                Lambda,
                Ident("y".into()),
                Arrow,
                Ident("x".into()),
                RParen,
                RParen,
                Number(1.0),
                Number(2.0)
            ]
        );
        assert_eq!(
            tokenize(
                r###"(λodd -> odd 3)
                            (λx -> equals (mod x 2) 1)"###
            ),
            vec![
                LParen,
                Lambda,
                Ident("odd".into()),
                Arrow,
                Ident("odd".into()),
                Number(3.0),
                RParen,
                LParen,
                Lambda,
                Ident("x".into()),
                Arrow,
                Ident("equals".into()),
                LParen,
                Ident("mod".into()),
                Ident("x".into()),
                Number(2.0),
                RParen,
                Number(1.0),
                RParen
            ]
        );
        assert_eq!(
            tokenize("(λx -> x y (λy -> y)) y"),
            vec![
                LParen,
                Lambda,
                Ident("x".into()),
                Arrow,
                Ident("x".into()),
                Ident("y".into()),
                LParen,
                Lambda,
                Ident("y".into()),
                Arrow,
                Ident("y".into()),
                RParen,
                RParen,
                Ident("y".into())
            ]
        );
        assert_eq!(
            tokenize("let x = 3 in odd x"),
            vec![
                Let,
                Ident("x".into()),
                Equal,
                Number(3.0),
                In,
                Ident("odd".into()),
                Ident("x".into())
            ]
        );
        assert_eq!(
            tokenize(
                r###"let x = (\x -> x)
                            in x (odd (x 3))"###
            ),
            vec![
                Let,
                Ident("x".into()),
                Equal,
                LParen,
                Lambda,
                Ident("x".into()),
                Arrow,
                Ident("x".into()),
                RParen,
                In,
                Ident("x".into()),
                LParen,
                Ident("odd".into()),
                LParen,
                Ident("x".into()),
                Number(3.0),
                RParen,
                RParen
            ]
        );
    }
}
