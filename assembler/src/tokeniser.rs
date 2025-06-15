use std::io::{BufRead, BufReader, Read};

use logos::Logos;

use crate::token::{LexError, Token, TokenStream, TokenType};

/// Tokenises the source code in `reader`.
///
/// # Errors
///
/// This can return an error if the error list is not empty. i.e. something went
/// wrong tokenising (usually parsing integers or because of unmatched tokens).
pub fn tokenise<R: Read>(reader: BufReader<R>) -> Result<TokenStream, Vec<LexError>> {
    let mut ts = TokenStream::new();
    let mut errs: Vec<LexError> = vec![];

    for (line_no, line) in reader.lines().enumerate() {
        let line_content = match line {
            Ok(line) => line,
            _ => continue,
        };

        let lexer = TokenType::lexer(line_content.as_str());

        for (token, span) in lexer.spanned() {
            match token {
                Ok(token_type) => ts.push(Token::new(line_no, span, token_type)),
                Err(why) => {
                    eprintln!(
                        "Tokeniser error at span {:?}: {:?} - \"{:#?}\"",
                        span,
                        why,
                        &line_content[span.clone()],
                    );

                    errs.push(why);
                    continue;
                }
            }
        }
    }

    if errs.is_empty() { Ok(ts) } else { Err(errs) }
}
