//! Some of the structures here are missing span info, this is because they're not as important
//! when constructing a string literal, since string literals can't contain comments.

use {
    crate::{
        formatter::{ChainingRule, FmtBlock, FmtCtx, Format, Located, Location, Spacing},
        utils::{default, Result},
    },
    either::Either,
    proc_macro2::{LineColumn, Span},
    shrimple_parser::{
        any,
        pattern::{parse, parse_until, parse_while},
        Parser as _, Pattern,
    },
    syn::{
        parse::{Parse, ParseStream},
        LitStr,
    },
};

pub fn is_inline_css_attr(element: &str, attr: &str) -> bool {
    match element {
        "a" | "abbr" | "article" | "aside" | "audio" | "b" | "blockquote" | "br" | "button"
        | "canvas" | "caption" | "cite" | "code" | "col" | "colgroup" | "details" | "div"
        | "dl" | "dt" | "dd" | "em" | "figcaption" | "figure" | "fieldset" | "footer" | "form"
        | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "header" | "hr" | "i" | "iframe" | "img"
        | "input" | "label" | "legend" | "li" | "main" | "mark" | "meter" | "nav" | "ol"
        | "option" | "p" | "pre" | "progress" | "section" | "select" | "small" | "span"
        | "strong" | "sub" | "summary" | "sup" | "table" | "tbody" | "td" | "textarea"
        | "tfoot" | "th" | "thead" | "time" | "tr" | "u" | "ul" | "video" => attr == "style",

        _ => false,
    }
}

pub struct StyleString {
    span: Span,
    is_raw: bool,
    n_escapes: u8,
    settings: Vec<Setting>,
}

#[derive(Debug)]
struct Setting {
    name: Location,
    value: Location,
}

impl Parse for StyleString {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let s: LitStr = input.parse()?;
        let span = s.span();
        let repr = s.token().to_string();

        let (is_raw, rest) = repr.strip_prefix('r').map_or((false, &*repr), |r| (true, r));
        let n_escapes = rest.bytes().take_while(|&b| b == b'#').count();
        let repr_start = span.start();
        let loc = shrimple_parser::Location {
            col: repr_start.column as u32 + is_raw as u32 + n_escapes as u32 + 1,
            line: (repr_start.line as u32).try_into().expect("non-zero line number"),
        };

        // exclude the escapes & the quotes
        let rest = &rest[n_escapes + 1 + is_raw as usize..rest.len() - n_escapes - 1];
        let n_escapes =
            u8::try_from(n_escapes).map_err(|_| syn::Error::new(span, "too many `#`"))?;
        let whitespace = if is_raw {
            Either::Left(['\t', '\n', '\r', ' '])
        } else {
            Either::Right(any!(['\t', '\n', '\x0C', '\r', ' '], "\\t", "\\n", "\\r", "\\\n"))
        };

        let (_, settings) = parse_while(whitespace)
            .then(parse_until(whitespace.or(':')).filter(|k: &&str| !k.is_empty()))
            .skip(parse_while(whitespace))
            .skip(parse(':').or_reason_if_nonempty("expected `:`"))
            .skip(parse_while(whitespace))
            .and(parse_until(whitespace.or(';')))
            .skip(parse_while(whitespace))
            .skip(parse(';').maybe())
            .map_out(|(k, v)| Setting {
                name: Location::find_saturating(loc, k, rest),
                value: Location::find_saturating(loc, v, rest),
            })
            .collect()
            .parse(rest)
            .map_err(|e| syn::Error::new(span, e.reason.unwrap_or("")))?;

        Ok(Self { span, settings, is_raw, n_escapes })
    }
}

impl Format for StyleString {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        let start = self.span.start();
        let end = self.span.end();
        let opening_quote = Location {
            start,
            end: LineColumn {
                line: start.line,
                column: start.column + self.is_raw as usize + self.n_escapes as usize + 1,
            },
        };
        let closing_quote = Location {
            start: LineColumn { line: end.line, column: end.column - self.n_escapes as usize - 1 },
            end,
        };
        block.add_delimited_block(
            ctx,
            opening_quote,
            closing_quote,
            Some(Spacing { between: true, ..default() }),
            ChainingRule::Off,
            |block, ctx| {
                for (i, setting) in self.settings.iter().enumerate() {
                    if i > 0 {
                        block.add_sep(ctx, setting.name.start())?;
                    }
                    block.add_source(ctx, setting.name)?;
                    block.add_text(ctx, ":", setting.name.end())?;
                    block.add_space(ctx, setting.value.start())?;
                    block.add_source(ctx, setting.value)?;
                    if i < self.settings.len() - 1 {
                        block.add_text(ctx, ";", setting.value.end())?;
                    }
                }
                Ok(())
            },
        )?;

        Ok(())
    }
}

impl Located for StyleString {
    fn start(&self) -> LineColumn {
        self.span.start()
    }

    fn end(&self) -> LineColumn {
        self.span.end()
    }
}
