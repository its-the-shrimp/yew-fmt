//! Some of the structures here are missing span info, this is because they're not as important
//! when constructing a string literal, since string literals can't contain comments.

use {
    crate::{
        config::Config,
        formatter::{ChainingRule, FmtBlock, FmtCtx, Format, Located, Location, Spacing},
        html::{
            base::{BaseHtmlFlavor, HtmlLiteralElement, HtmlProp, HtmlPropKind},
            ext::ExtHtmlFlavor,
            visitor::{self, Visitor},
            Html,
        },
        utils::{default, Result},
    },
    either::Either,
    proc_macro2::{LineColumn, Span},
    shrimple_parser::{
        any,
        pattern::{parse, parse_until, parse_while, NotEnclosed},
        Parser as _, Pattern,
    },
    std::{fmt::Write, mem::take},
    syn::{Lit, LitStr},
};

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

impl StyleString {
    pub fn parse(input: &LitStr) -> syn::Result<Self> {
        let span = input.span();
        let repr = input.token().to_string();

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
            .and(
                parse_until(NotEnclosed('"', ';'))
                    .map_out(|v| whitespace.trailing_matches_counted(v).0),
            )
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

struct CssParser<'config> {
    config: &'config Config,
    element_name: String,
    attr_name: String,
    error: syn::Result<()>,
}

impl Visitor for CssParser<'_> {
    fn visit_base_html_literal_element(
        &mut self,
        literal: &mut HtmlLiteralElement<BaseHtmlFlavor>,
    ) {
        self.element_name.clear();
        _ = write!(self.element_name, "{}", literal.name);
        visitor::visit_base_html_literal_element(self, literal);
    }

    fn visit_ext_html_literal_element(&mut self, literal: &mut HtmlLiteralElement<ExtHtmlFlavor>) {
        self.element_name.clear();
        _ = write!(self.element_name, "{}", literal.name);
        visitor::visit_ext_html_literal_element(self, literal);
    }

    fn visit_html_prop(&mut self, prop: &mut HtmlProp) {
        let (name, eq, style) = match &mut prop.kind {
            HtmlPropKind::Literal(name, eq, Lit::Str(v)) => {
                self.attr_name.clear();
                _ = write!(self.attr_name, "{}", name[0].0);
                if !self.config.is_inline_css_attr(&self.element_name, &self.attr_name) {
                    return;
                }

                let style = match StyleString::parse(v) {
                    Ok(style) => style,
                    Err(e) => {
                        if self.error.is_ok() {
                            self.error = Err(e);
                        }
                        return;
                    }
                };
                (take(name), *eq, style)
            }

            _ => return,
        };

        prop.kind = HtmlPropKind::Style(name, eq, style);
    }
}

pub fn parse_css(html: &mut Html, config: &Config) -> syn::Result<()> {
    if !config.yew.format_css {
        return Ok(());
    }

    let mut parser =
        CssParser { config, element_name: String::new(), attr_name: String::new(), error: Ok(()) };
    parser.visit_html(html);
    parser.error
}
