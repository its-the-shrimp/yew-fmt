use std::fmt::Write;
use anyhow::{Context, Result, bail};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term::{emit, termcolor::{StandardStream, ColorChoice}, Config};
use codespan_reporting::files::SimpleFile;
use proc_macro2::LineColumn;
use syn::Expr;
use syn::{spanned::Spanned, Macro, visit::Visit};
use crate::html::*;

trait Format<'src> {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()>;
}

impl<'src> Format<'src> for Html {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        match self {
            Html::Tree(tree) => tree.format(block, ctx),
            Html::Value(val) => val.format(block, ctx),
        }
    }
}

impl<'src> Format<'src> for HtmlTree {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        match self {
            HtmlTree::Element(e) => e.format(block, ctx),
            HtmlTree::Block(b) => b.format(block, ctx),
            HtmlTree::If(i) => i.format(block, ctx),
        }
    }
}

impl<'src> Format<'src> for HtmlElement {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        match self {
            HtmlElement::Fragment(f) => f.format(block, ctx),
            HtmlElement::Dynamic(d) =>  d.format(block, ctx),
            HtmlElement::Literal(l) =>  l.format(block, ctx),
        }
    }
}

impl<'src> Format<'src> for HtmlFragment {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        block.add_spanned(ctx, &self.lt_token)?;
        if let Some(key) = &self.key {
            key.format(block, ctx)?;
        }
        block.add_spanned(ctx, &self.gt_token)?;

        block.add_block(FmtSep::None, |block| anyhow::Ok({
            for child in &self.children {
                child.format(block, ctx)?;
                block.add_sep();
            }
        }))?;

        block.add_spanned(ctx, &self.closing_lt_token)?;
        block.add_spanned(ctx, &self.div_token)?;
        block.add_spanned(ctx, &self.closing_gt_token)
    }
}

impl<'src> Format<'src> for HtmlDynamicElement {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        block.add_text("<@");
        self.name.format(block, ctx)?;

        block.add_block(FmtSep::Space, |block| anyhow::Ok({
            for prop in &self.props {
                prop.format(block, ctx)?;
                block.add_sep();
            }
        }))?;

        Ok(if self.closing_tag.is_some() {
            block.add_text(">");
            block.add_block(FmtSep::None, |block| anyhow::Ok({
                for child in &self.children {
                    child.format(block, ctx)?;
                    block.add_sep();
                }
            }))?;
            block.add_text("</@>");
        } else {
            block.add_text("/>");
        })
    }
}

impl<'src> Format<'src> for HtmlLiteralElement {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        block.add_text("<");
        block.add_spanned_iter(ctx, self.name.clone())?;

        block.add_block(FmtSep::Space, |block| anyhow::Ok({
            for prop in &self.props {
                prop.format(block, ctx)?;
                block.add_sep();
            }
        }))?;
        
        if let Some((_, base)) = &self.prop_base {
            block.add_text("..");
            block.add_spanned(ctx, base)?;
        }

        Ok(if self.closing_tag.is_some() {
            block.add_text(">");
            block.add_block(FmtSep::None, |block| anyhow::Ok({
                for child in &self.children {
                    child.format(block, ctx)?;
                    block.add_sep();
                }
            }))?;
            block.add_text("</");
            block.add_spanned_iter(ctx, self.name.clone())?;
            block.add_text(">");
        } else {
            block.add_text("/>");
        })
    }
}

impl<'src> Format<'src> for HtmlProp {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        if self.access_spec.is_some() {
            block.add_text("~");
        }
        match &self.kind {
            HtmlPropKind::Shortcut(_, name) => {
                block.add_text("{");
                block.add_spanned_with_sep(ctx, name.iter(), "-")?;
                Ok(block.add_text("}"))
            }
            HtmlPropKind::Literal(name, _, lit) => {
                block.add_spanned_with_sep(ctx, name.iter(), "-")?;
                block.add_text("=");
                block.add_spanned(ctx, &lit)
            }
            HtmlPropKind::Expr(name, _, expr) => {
                block.add_spanned_with_sep(ctx, name.iter(), "-")?;
                block.add_text("=");
                if matches!(expr.expr, Expr::Lit(_)) {
                    block.add_spanned(ctx, &expr.expr)
                } else {
                    expr.format(block, ctx)
                }
            }
        }
    }
}

impl<'src> Format<'src> for BracedExpr {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        block.add_text("{");
        block.add_spanned(ctx, &self.expr)?;
        Ok(block.add_text("}"))
    }
}

impl<'src> Format<'src> for HtmlBlock {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        block.add_text("{ ");
        self.content.format(block, ctx)?;
        Ok(block.add_text(" }"))
    }
}

impl<'src> Format<'src> for HtmlBlockContent {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        match self {
            Self::Expr(e) => block.add_spanned(ctx, e),
            Self::Iterable(_, e) => {
                block.add_text("for ");
                block.add_spanned(ctx, e)
            }
        }
    }
}

impl<'src> Format<'src> for HtmlIf {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        block.add_text("if ");
        block.add_spanned(ctx, &self.condition)?;
        block.add_text(" {");
        block.add_block(FmtSep::None, |block| anyhow::Ok({
            for child in &self.then_branch {
                child.format(block, ctx)?;
                block.add_sep();
            }
        }))?;

        if let Some(else_branch) = &self.else_branch {
            block.add_text("} ");
            else_branch.format(block, ctx)
        } else {
            Ok(block.add_text("}"))
        }
    }
}

impl<'src> Format<'src> for HtmlElse {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        block.add_text("else ");
        Ok(match self {
            Self::If(_, r#if) => r#if.format(block, ctx)?,
            Self::Tree(.., children) => {
                block.add_text("{");
                block.add_block(FmtSep::None, |block| anyhow::Ok({
                    for child in children {
                        child.format(block, ctx)?;
                        block.add_sep();
                    }
                }))?;
                block.add_text("}");
            }
        })
    }
}

/// Doesn't store any context by itself, exists only to reuse allocations effeciently
#[derive(Default)]
pub struct Formatter {
    /// maps line number to byte offset in `input`
    offsets: Vec<usize>,
    /// stack with starting indices of formatting blocks
    block_starts: Vec<usize>,
    /// for temporary strings to reuse allocations
    temp_str_buf: String,
    /// the formatted code
    output: String,
}

/// Represents the way in which to break up adjacent tokens
#[derive(Clone, Copy, Default, PartialEq, Eq)]
#[repr(usize)]
enum FmtSep {
    #[default]
    None,
    Space,
    Newline,
}

impl FmtSep {
    fn len(&self) -> usize {
        matches!(self, Self::Space) as usize
    }

    fn print(&self, indent: usize, out: &mut String) {
        match self {
            FmtSep::None => (),
            FmtSep::Space => out.push(' '),
            FmtSep::Newline => {
                out.reserve(indent + 1);
                out.push('\n');
                for _ in 0 .. indent {
                    out.push(' ');
                }
            }
        }
    }
}

/// Represents text that's not yet written: text, space, or a group of those
// TODO: handle comments
#[derive(PartialEq, Eq)]
enum FmtToken<'src> {
    Text(&'src str),
    Sep,
    Block(FmtBlock<'src>)
}

#[derive(PartialEq, Eq, Default)]
struct FmtBlock<'src> {
    sep: FmtSep,
    // TODO: find a way to reuse this allocation
    tokens: Vec<FmtToken<'src>>,
    width: usize
}

impl FmtToken<'_> {
    fn width(&self, sep: FmtSep) -> usize {
        match *self {
            Self::Text(text) => text.len(),
            Self::Sep => sep.len(),
            Self::Block(FmtBlock { width, .. }) => width,
        }
    }

    fn print(&self, indent: usize, sep: FmtSep, out: &mut String) {
        match self {
            Self::Text(text) => out.push_str(text),
            Self::Sep => sep.print(indent, out),
            Self::Block(block) => block.print(indent, out)
        }
    }
}

impl<'src> FmtBlock<'src> {
    fn add_text(&mut self, text: &'src str) {
        self.tokens.push(FmtToken::Text(text));
        self.width += text.len();
    }

    fn add_sep(&mut self) {
        self.tokens.push(FmtToken::Sep);
        self.width += self.sep.len();
    }

    /// adds a block and gives a mutable reference to it to `f`
    fn add_block<R>(&mut self, sep: FmtSep, f: impl FnOnce(&mut Self) -> R) -> R {
        let mut block = Self { sep, tokens: vec![], width: 0 };
        let res = f(&mut block);
        if block.tokens.last() == Some(&FmtToken::Sep) {
            block.tokens.pop();
        }
        self.width += block.width;
        self.tokens.push(FmtToken::Block(block));
        res
    }

    fn add_spanned(&mut self, ctx: &FormatCtx<'_, 'src>, token: &impl Spanned) -> Result<()> {
        let span = token.span();
        let text = ctx.source_code(span.start(), span.end())
            .context("failed to get a token's source code")?;
        Ok(self.add_text(text))
    }

    fn add_spanned_iter(
        &mut self,
        ctx: &FormatCtx<'_, 'src>,
        iter: impl IntoIterator<Item = impl Spanned>,
    ) -> Result<()> {
        Ok(for obj in iter {
            self.add_spanned(ctx, &obj)?;
        })
    }

    fn add_spanned_with_sep(
        &mut self,
        ctx: &FormatCtx<'_, 'src>,
        iter: impl IntoIterator<Item = impl Spanned>,
        sep: &'src str,
    ) -> Result<()> {
        let mut iter = iter.into_iter();
        if let Some(first) = iter.next() {
            self.add_spanned(ctx, &first)?;
        }
        for obj in iter {
            self.add_text(sep);
            self.add_spanned(ctx, &obj)?;
        }
        Ok(())
    }

    fn determine_breaking(&mut self, offset: usize, indent: usize) {
        let mut max_width = offset + indent;
        for token in &self.tokens {
            max_width += token.width(self.sep);
        }
        if max_width > Formatter::MAX_WIDTH {
            self.sep = FmtSep::Newline;
            self.width = 0;
            let indent = indent + Formatter::INDENT_STEP;
            let mut offset = 0;
            for token in &mut self.tokens {
                match token {
                    FmtToken::Text(text) => offset += text.len(),
                    FmtToken::Sep => match self.sep {
                        FmtSep::None => (),
                        FmtSep::Space => offset += 1,
                        FmtSep::Newline => offset = 0,
                    }
                    FmtToken::Block(block) => {
                        block.determine_breaking(offset, indent);
                        offset += block.width;
                    }
                }
            }
        }
    }

    fn print(&self, indent: usize, out: &mut String) {
        let new_indent = indent
            + if self.sep == FmtSep::Newline {Formatter::INDENT_STEP} else {0};
        if !self.tokens.is_empty() {
            self.sep.print(new_indent, out);
            for token in &self.tokens {
                token.print(new_indent, self.sep, out);
            }
            if self.sep == FmtSep::Newline {
                FmtSep::Newline.print(indent, out)
            }
        }
    }
}

struct FormatCtx<'fmt, 'src> {
    /// for error reporting purposes
    filename: &'src str,
    /// maps line number to byte offset in `input`
    offsets: &'fmt mut Vec<usize>,
    /// stack of starting indices of formatting blocks
    block_starts: &'fmt mut Vec<usize>,
    /// for temporary strings to reuse allocations
    temp_str_buf: &'fmt mut String,
    /// the formatted code
    output: &'fmt mut String,
    /// the source code
    input: &'src str,
    /// to return errors from within AST traversal
    err: Result<Option<Diagnostic<()>>>,
    /// the end of `output` represented as byte offset into `input`
    cur_offset: usize,
    /// the end of `output` represented as position in `input`
    cur_pos: LineColumn,
}

impl Visit<'_> for FormatCtx<'_, '_> {
    // TODO: rewrite with a `try` block when those get stabilised
    fn visit_macro(&mut self, i: &Macro) {
        self.err = (|| -> Result<Option<Diagnostic<()>>> {
            if let Some(macro_name) = i.path.segments.last() {
                write!(self.temp_str_buf, "{}", macro_name.ident)
                    .context("failed to get invoked macro path")?;
            }
            let is_html = matches!(self.temp_str_buf.as_str(), "html" | "html_nested");
            self.temp_str_buf.clear();
            if !is_html {return Ok(None)}

            let span = i.delimiter.span();
            let (opening_span, closing_span) = (span.open(), span.close());
            self.print_source(opening_span.start())?;
            let html = match syn::parse2::<Html>(i.tokens.clone()) {
                Ok(html) => html,
                Err(e) => {
                    let span = e.span();
                    let start = self.pos_to_byte_offset(span.start())?;
                    let end = self.pos_to_byte_offset(span.end())?;
                    return Ok(Some(
                        Diagnostic::error()
                            .with_message(e.to_string())
                            .with_labels(vec![Label::primary((), start .. end)])
                    ))
                }
            };
            let mut block = FmtBlock::default();
            html.format(&mut block, self)?;

            self.print_text("{", opening_span.end())?;
            self.print_fmt_block(block, closing_span.start())?;
            self.print_text("}", closing_span.end())?;
            Ok(None)
        })();
    }
}

impl Formatter {
    const INDENT_STEP: usize = 4;
    const MAX_WIDTH: usize = 100;

    pub fn format<'fmt, 'src: 'fmt>(
        &'fmt mut self,
        filename: &'src str,
        input: &'src str
    ) -> Result<FormatResult<'fmt, 'src>> {
        self.output.clear();
        let mut ctx = FormatCtx {
            offsets: &mut self.offsets,
            block_starts: &mut self.block_starts,
            temp_str_buf: &mut self.temp_str_buf,
            output: &mut self.output,
            filename,
            input,
            err: Ok(None),
            cur_offset: 0,
            cur_pos: LineColumn { line: 1, column: 0 }
        };
        let file = syn::parse_file(input)?;
        ctx.offsets.push(0);
        ctx.offsets.extend(input.char_indices()
            .filter_map(|(i, c)| (c == '\n').then_some(i + 1)));

        ctx.visit_file(&file);
        ctx.finalise()
    }
}

impl<'fmt, 'src> FormatCtx<'fmt, 'src> {
    fn pos_to_byte_offset(&self, LineColumn{line, column}: LineColumn) -> Result<usize> {
        self.offsets.get(line.saturating_sub(1))
            .with_context(|| format!("line {line} doesn't exist in the source file"))?
            .checked_add(column)
            .with_context(||
                format!("source position {line}:{column} can't be converted to a byte offset"))
    }

    fn source_code(&self, start: LineColumn, end: LineColumn) -> Result<&'src str> {
        let start = self.pos_to_byte_offset(start)
            .context("failed to find the start of the span")?;
        let end = self.pos_to_byte_offset(end)
            .context("failed to find the end of the span")?;
        self.input.get(start .. end)
            .with_context(|| format!("byte range {start}..{end} is invalid for the source code"))
    }

    fn line_indent(&self, line: usize) -> Result<usize> {
        enum State {
            Space,
            CommentStart,
            Comment,
            CommentEnd
        }
        let mut state = State::Space;

        let &start = self.offsets.get(line - 1)
            .with_context(|| format!("line {line} doesn't exist in the source file"))?;
        let line = unsafe{ self.input.get_unchecked(start ..) };
        for (i, ch) in line.char_indices() {
            match ch {
                ' ' => state = match state {
                    State::Space => continue,
                    State::CommentStart => State::Space,
                    State::Comment => continue,
                    State::CommentEnd => State::Space
                },
                '/' => state = match state {
                    State::Space => State::CommentStart,
                    State::CommentStart => bail!("line {line} of the source file is empty"),
                    State::Comment => continue,
                    State::CommentEnd => State::Space,
                },
                '*' => state = match state {
                    State::Space => continue,
                    State::CommentStart => State::Comment,
                    State::Comment => State::CommentEnd,
                    State::CommentEnd => continue,
                },
                '\n' => bail!("line {line} of the source file is empty"),
                _ => match state {
                    State::Space => return Ok(i),
                    State::CommentStart => return Ok(i - 1),
                    State::Comment => continue,
                    State::CommentEnd => continue,
                }
            }
        }
        bail!("line {line} of the source file is empty")
    }

    fn print_source(&mut self, until: LineColumn) -> Result<()> {
        let until_byte = self.pos_to_byte_offset(until)?;
        let from = self.cur_offset;
        let new = self.input.get(from .. until_byte)
            .with_context(||
                format!("range {from} .. {until_byte} is out of bounds for the source file"))?;
        self.cur_offset = until_byte;
        self.cur_pos = until;
        Ok(self.output.push_str(new))
    }

    // `end` is the position in the source file asssumed to be the end of the text
    fn print_text(&mut self, text: &str, end: LineColumn) -> Result<()> {
        self.output.push_str(text);
        self.cur_pos = end;
        let off = self.pos_to_byte_offset(end)?;
        Ok(self.cur_offset = off)
    }

    // `end` is the position in the source file asssumed to be the end of the formatted sequence
    fn print_fmt_block(&mut self, mut block: FmtBlock<'src>, end: LineColumn) -> Result<()> {
        let indent = self.line_indent(self.cur_pos.line)?;
        block.determine_breaking(self.cur_pos.column - indent, indent);
        block.print(indent, self.output);
        self.cur_pos = end;
        let off = self.pos_to_byte_offset(end)?;
        Ok(self.cur_offset = off)
    }

    fn finalise(self) -> Result<FormatResult<'fmt, 'src>> {
        let rest = unsafe { self.input.get_unchecked(self.cur_offset ..) };
        self.offsets.clear();
        self.block_starts.clear();
        self.output.push_str(rest);
        self.err.map(|diagnostic| FormatResult {
            filename: self.filename,
            source: self.input,
            output: match diagnostic {
                Some(diagnostic) => Err(diagnostic),
                None => Ok(self.output.as_str())
            }
        })
    }
}

pub struct FormatResult<'fmt, 'src> {
    filename: &'src str,
    source: &'src str,
    output: Result<&'fmt str, Diagnostic<()>>
}

impl<'fmt, 'src> FormatResult<'fmt, 'src> {
    /// if the result is an error, write it into stderr, if it's successfully formatted code,
    /// return it
    pub fn emit_error(self) -> Result<Option<&'fmt str>> {
        let diagnostic = match self.output {
            Ok(out) => return Ok(Some(out)),
            Err(x) => x,
        };
        let mut writer = StandardStream::stderr(ColorChoice::Auto);
        emit(
            &mut writer,
            &Config::default(),
            &SimpleFile::new(self.filename, self.source),
            &diagnostic
        )?;
        Ok(None)
    }
}
