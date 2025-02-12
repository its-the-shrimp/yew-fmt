use crate::config::Config;
use crate::utils::{default, Result, SliceExt, StrExt};
use anyhow::{bail, Context};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::WriteColor;
use proc_macro2::LineColumn;
use std::mem::{replace, take};
use std::vec::Vec as StdVec;
use syn::punctuated::Punctuated;
use syn::{spanned::Spanned, visit::Visit, Macro};
use syn::{Attribute, Item, MacroDelimiter, Stmt};

fn is_skipped(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|attr| attr.path().segments.iter().map(|x| &x.ident).eq(["rustfmt", "skip"]))
}

/// if `new` is 1 line, returns its length added to `prev`, otherwise returns the length of the
/// last line of `new`.
fn add_last_line_len(prev: usize, new: &str) -> usize {
    new.last_line_len().unwrap_or(new.len() + prev)
}

#[derive(Debug, Clone, Copy)]
enum Comment<'src> {
    /// the initial `//` and the newline are not included
    Line(&'src str),
    /// the `/*` and `*/` are included
    Multi(&'src str),
}

struct CommentParser<'src>(&'src str);

impl<'src> Iterator for CommentParser<'src> {
    type Item = Comment<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        /// the `usize`s are offsets into `src`
        #[derive(Clone, Copy)]
        enum ParserState {
            None,
            Start(usize),
            Line(usize),
            Multi(usize),
            MultiEnd(usize),
        }

        let Self(src) = self;
        let mut state = ParserState::None;
        for (i, c) in src.char_indices() {
            match c {
                '/' => match state {
                    ParserState::None => state = ParserState::Start(i),
                    ParserState::Start(_) => state = ParserState::Line(i + 1),
                    ParserState::MultiEnd(start) => unsafe {
                        // Safety: `src[i]` is guaranteed to be '/'
                        let (extracted, rest) = src.split_at_unchecked(i + 1);
                        *src = rest;
                        return Some(Comment::Multi(extracted.get_unchecked(start..)));
                    },
                    _ => (),
                },

                '*' => match state {
                    ParserState::Start(i) => state = ParserState::Multi(i),
                    ParserState::Multi(i) => state = ParserState::MultiEnd(i),
                    _ => (),
                },

                '\n' => match state {
                    ParserState::Start(_) => state = ParserState::None,
                    ParserState::Line(start) => unsafe {
                        let (extracted, rest) = src.split_at_unchecked(i);
                        *src = rest;
                        let res = Some(Comment::Line(extracted.get_unchecked(start..)));
                        return res;
                    },
                    ParserState::MultiEnd(i) => state = ParserState::Multi(i),
                    _ => (),
                },

                _ => match state {
                    ParserState::Start(_) => state = ParserState::None,
                    ParserState::MultiEnd(i) => state = ParserState::Multi(i),
                    _ => (),
                },
            }
        }

        *src = "";
        None
    }
}

#[derive(Clone, Copy)]
pub struct Location {
    pub start: LineColumn,
    pub end: LineColumn,
}

/// Represents an object that has an associated location in the source
pub trait Located {
    fn start(&self) -> LineColumn;
    fn end(&self) -> LineColumn;
    fn loc(&self) -> Location {
        Location { start: self.start(), end: self.end() }
    }
}

impl<T: Spanned> Located for T {
    fn start(&self) -> LineColumn {
        self.span().start()
    }
    fn end(&self) -> LineColumn {
        self.span().end()
    }
    fn loc(&self) -> Location {
        let span = self.span();
        Location { start: span.start(), end: span.end() }
    }
}

impl Located for Location {
    fn start(&self) -> LineColumn {
        self.start
    }
    fn end(&self) -> LineColumn {
        self.end
    }
    fn loc(&self) -> Location {
        *self
    }
}

pub trait Format {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result;
    /// equivalent to:
    /// ```rust,ignore
    /// block.add_space(ctx, self.start())?;
    /// self.format(block, ctx)?;
    /// ```
    /// but does so more concisely
    fn format_with_space<'src>(
        &self,
        block: &mut FmtBlock<'_, 'src>,
        ctx: &mut FmtCtx<'_, 'src>,
    ) -> Result
    where
        Self: Located,
    {
        block.add_space(ctx, self.start())?;
        self.format(block, ctx)
    }
}

/// Stores the config and allocated memory to reuse it between reformatting
pub struct Formatter {
    config: Config,
    /// buffer for tokens stored in `FmtBlock`s
    tokens_buf: Bump,
    /// maps line number to byte offset in `input`
    offsets: StdVec<usize>,
    /// the formatted code
    output: String,
}

/// Represents text that's not yet written: text, space, or a group of those
#[derive(Debug)]
enum FmtToken<'fmt, 'src> {
    Text(&'src str),
    /// needs special handling of the newline
    LineComment(&'src str),
    /// The contained integer is the number of newlines to be put if the parent block is broken up.
    /// Added for compatibility with rustfmt's formatting of match expressions
    Sep(u8),
    Block(FmtBlock<'fmt, 'src>),
}

#[derive(Clone, Copy, PartialEq, Eq, Default, Debug)]
pub struct Spacing {
    pub before: bool,
    pub between: bool,
    pub after: bool,
}

impl Spacing {
    pub const AROUND: Self = Self { before: true, between: false, after: true };
}

/// Chains are sequences of formatting blocks that are all broken if one of them is broken; in a
/// sequence of formatting blocks, a chain will have the following shape:
/// `[..., Off, Full, Forward, ..., Full, End, Off, ...]`
/// where the words are the names of the variants of [`ChainingRule`].
/// A chain starts from a [`ChainingRule::On`] variant but ends with a [`ChainingRule::End`]; this
/// is done to make the chains declare their end on their own without having to add an addition
/// variant to [`FmtToken`], and to also avoid the possibility of 2 unrelated chains getting
/// misinterpreted as 1.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ChainingRule {
    /// no chaining
    Off,
    /// ends a chain (while being inside it itself) and does not break the blocks before it in the
    /// chain if broken
    End,
    /// inside a chain and breaks the blocks before it in the chain if broken
    On,
}

impl ChainingRule {
    pub const fn is_on(&self) -> bool {
        matches!(self, Self::On)
    }
}

#[derive(Debug)]
pub struct FmtBlock<'fmt, 'src> {
    tokens: Vec<'fmt, FmtToken<'fmt, 'src>>,
    width: usize,
    chaining_rule: ChainingRule,
    /// if `None`, the block is broken, since spacing only matters when the block isn't broken
    spacing: Option<Spacing>,
    /// offset into the source, useful for correct printing of comments
    cur_offset: usize,
}

impl<'fmt, 'src> FmtBlock<'fmt, 'src> {
    fn new(
        alloc: &'fmt Bump,
        spacing: Option<Spacing>,
        chaining: ChainingRule,
        start_offset: usize,
    ) -> Self {
        Self {
            tokens: Vec::new_in(alloc),
            width: 0,
            spacing,
            cur_offset: start_offset,
            chaining_rule: chaining,
        }
    }

    // Functions for adding all the token kinds directly; not to be exposed

    fn add_raw_text(&mut self, text: &'src str) {
        match text.bytes().filter(|&b| b == b'\n').count() {
            0 => self.width += text.len(),
            _ => self.spacing = None,
        }
        self.tokens.push(FmtToken::Text(text))
    }

    fn add_line_comment(&mut self, comment: &'src str) {
        self.width += comment.len() + 4;
        self.tokens.push(FmtToken::LineComment(comment))
    }

    fn add_raw_sep(&mut self, n_newlines: u8) {
        self.width += self.spacing.is_some_and(|s| s.between) as usize;
        self.tokens.push(FmtToken::Sep(n_newlines))
    }

    fn add_raw_block(&mut self, mut block: FmtBlock<'fmt, 'src>) {
        if matches!(block.tokens.last(), Some(FmtToken::Sep(_))) {
            block.tokens.pop();
        }
        match block.spacing {
            Some(_) => self.width += block.width,
            None => self.spacing = None,
        }
        self.cur_offset = block.cur_offset;
        self.tokens.push(FmtToken::Block(block))
    }

    // Utilities

    fn add_comments_with_sep(
        &mut self,
        ctx: &FmtCtx<'_, 'src>,
        until: LineColumn,
        mut sep: impl FnMut(&mut Self),
    ) -> Result {
        let until = ctx.pos_to_byte_offset(until)?;
        let range = self.cur_offset..until;
        let comment = ctx
            .input
            .get(range.clone())
            .with_context(|| format!("span {range:?} is out of bounds for the source"))?;
        self.cur_offset = until;

        let mut comment_added = false;
        for comment in CommentParser(comment) {
            if replace(&mut comment_added, true) {
                sep(self);
            }
            match comment {
                Comment::Line(line) => self.add_line_comment(line),
                Comment::Multi(inner) => self.add_raw_text(inner),
            }
        }

        if comment_added && self.spacing.is_some_and(|s| s.between) {
            sep(self);
        }
        Ok(())
    }

    pub fn add_comments(&mut self, ctx: &FmtCtx<'_, 'src>, until: LineColumn) -> Result {
        self.add_comments_with_sep(ctx, until, |b| b.add_raw_sep(0))
    }

    pub fn add_space(&mut self, ctx: &FmtCtx<'_, 'src>, at: LineColumn) -> Result {
        self.add_raw_text(" ");
        self.add_comments(ctx, at)
    }

    pub fn add_text(&mut self, ctx: &FmtCtx<'_, 'src>, text: &'src str, at: LineColumn) -> Result {
        self.add_comments(ctx, at)?;
        self.add_raw_text(text);
        self.cur_offset += text.len();
        Ok(())
    }

    pub fn add_sep(&mut self, ctx: &FmtCtx<'_, 'src>, at: LineColumn) -> Result {
        self.add_raw_sep(1);
        self.add_comments_with_sep(ctx, at, |b| b.add_raw_sep(1))
    }

    /// Scans the source code from `at` until the first non-whitespace character, counts the number
    /// of newlines in that interval, confines that to the provided range and adds a
    /// separator with that number of newlines.
    pub fn add_aware_sep(
        &mut self,
        ctx: &FmtCtx<'_, 'src>,
        at: LineColumn,
        min_n_newlines: u8,
        max_n_newlines: u8,
    ) -> Result {
        let src_start = ctx.pos_to_byte_offset(at)?;
        let n_newlines = ctx
            .input
            .get(src_start..)
            .with_context(|| format!("invalid byte offset: {src_start}"))?
            .chars()
            .take_while(char::is_ascii_whitespace)
            .filter(|&c| c == '\n')
            .count();
        self.add_raw_sep(u8::try_from(n_newlines)?.clamp(min_n_newlines, max_n_newlines));
        self.add_comments_with_sep(ctx, at, |b| b.add_raw_sep(1))?;
        Ok(())
    }

    /// adds a block and gives a mutable reference to it to `f`
    pub fn add_block<R>(
        &mut self,
        spacing: Option<Spacing>,
        chaining: ChainingRule,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        let mut block = Self::new(self.tokens.bump(), spacing, chaining, self.cur_offset);
        let res = f(&mut block);
        self.add_raw_block(block);
        res
    }

    /// same as `add_block` but also:
    /// - adds the delimiters at the provided source locations;
    /// - adds trailing comments before the closing delimiter to the new block
    pub fn add_delimited_block<R>(
        &mut self,
        ctx: &mut FmtCtx<'_, 'src>,
        opening: impl Located,
        closing: impl Located,
        spacing: Option<Spacing>,
        chaining: ChainingRule,
        f: impl FnOnce(&mut Self, &mut FmtCtx<'_, 'src>) -> Result<R>,
    ) -> Result<R> {
        let closing = closing.loc();
        self.add_source(ctx, opening)?;
        let res = self.add_block(spacing, chaining, |block| {
            let res = f(block, ctx)?;
            block.add_comments(ctx, closing.start)?;
            anyhow::Ok(res)
        })?;
        self.add_source(ctx, closing).map(|_| res)
    }

    /// - adds a space to the current block before the opening delimiter
    pub fn add_delimited_block_with_space<R>(
        &mut self,
        ctx: &mut FmtCtx<'_, 'src>,
        opening: impl Located,
        closing: impl Located,
        spacing: Option<Spacing>,
        chaining: ChainingRule,
        f: impl FnOnce(&mut Self, &mut FmtCtx<'_, 'src>) -> Result<R>,
    ) -> Result<R> {
        let opening = opening.loc();
        self.add_space(ctx, opening.start)?;
        self.add_delimited_block(ctx, opening, closing, spacing, chaining, f)
    }

    pub fn add_source(&mut self, ctx: &FmtCtx<'_, 'src>, at: impl Located) -> Result {
        let loc = at.loc();
        let text = ctx.source_code(loc).context("failed to get a token's source code")?;
        self.add_text(ctx, text, loc.start)
    }

    /// equivalent to:
    /// ```rust,ignore
    /// let loc = ...;
    /// block.add_space(ctx, loc.start())?;
    /// block.add_source(ctx, loc)?;
    /// ```
    /// but does so more concisely
    pub fn add_source_with_space(&mut self, ctx: &FmtCtx<'_, 'src>, at: impl Located) -> Result {
        let loc = at.loc();
        self.add_space(ctx, loc.start)?;
        self.add_source(ctx, loc)
    }

    pub fn add_source_iter(
        &mut self,
        ctx: &FmtCtx<'_, 'src>,
        iter: impl IntoIterator<Item = impl Located>,
    ) -> Result {
        for obj in iter {
            self.add_source(ctx, obj)?;
        }
        Ok(())
    }

    pub fn add_source_punctuated<T, P>(
        &mut self,
        ctx: &FmtCtx<'_, 'src>,
        iter: &Punctuated<T, P>,
    ) -> Result
    where
        for<'any> &'any T: Located,
        for<'any> &'any P: Located,
    {
        for pair in iter.pairs() {
            let (value, punct) = pair.into_tuple();
            self.add_source(ctx, value)?;
            self.add_source_iter(ctx, punct)?;
        }
        Ok(())
    }

    fn force_breaking(&mut self, ctx: &FmtCtx<'_, 'src>, indent: usize) {
        self.spacing = None;
        self.width = 0;
        let mut offset = 0;
        let indent = indent + ctx.config.tab_spaces;
        let mut chain_broken = false;
        let mut tokens_iter = self.tokens.iter_with_prev_mut();
        while let Some((token, prev_tokens)) = tokens_iter.next() {
            match token {
                FmtToken::Text(text) => offset = add_last_line_len(offset, text),
                FmtToken::Sep(_) | FmtToken::LineComment(_) => offset = 0,
                FmtToken::Block(block) => {
                    if chain_broken {
                        block.force_breaking(ctx, indent);
                        chain_broken = block.chaining_rule.is_on()
                    } else if block.determine_breaking(ctx, offset, indent) {
                        if block.chaining_rule.is_on() {
                            for token in prev_tokens.iter_mut().rev() {
                                let FmtToken::Block(block) = token else { continue };
                                if !block.chaining_rule.is_on() {
                                    break;
                                };
                                block.force_breaking(ctx, indent)
                            }
                            chain_broken = true
                        }
                        offset = 0
                    } else {
                        offset += block.width
                    }
                }
            }
            if !chain_broken && indent + offset > ctx.config.yew.html_width {
                let mut first = true;
                offset = 0;
                for token in prev_tokens.iter_mut().rev() {
                    match token {
                        FmtToken::Text(text) => offset = add_last_line_len(offset, text),
                        FmtToken::LineComment(comment) => offset += comment.len() + 4,
                        FmtToken::Sep(_) => break,
                        FmtToken::Block(block) => {
                            if take(&mut first) {
                                chain_broken = block.chaining_rule.is_on();
                                block.force_breaking(ctx, indent);
                                offset = 0
                            } else if chain_broken {
                                block.force_breaking(ctx, indent);
                                offset = 0
                            } else {
                                offset += block.width
                            }
                        }
                    }
                }
            }
        }
    }

    /// the returned boolean indicates whether the block was broken or not
    fn determine_breaking(&mut self, ctx: &FmtCtx<'_, 'src>, offset: usize, indent: usize) -> bool {
        let Some(spacing) = self.spacing else {
            self.force_breaking(ctx, indent);
            return true;
        };

        if offset
            + indent
            + self.width
            + ((spacing.before || spacing.after) && !self.tokens.is_empty()) as usize
            >= ctx.config.yew.html_width
        {
            self.force_breaking(ctx, indent);
            return true;
        }

        false
    }

    fn print(&self, indent: usize, cfg: &Config, out: &mut String) {
        #[derive(Clone, Copy)]
        enum Sep {
            None,
            Space,
            Newline,
        }

        let space_if = |c| if c { Sep::Space } else { Sep::None };

        let print_token = |token: &FmtToken, out: &mut String, indent, sep| match token {
            FmtToken::Text(text) => out.push_str(text),
            FmtToken::LineComment(comment) => {
                if let Sep::Newline = sep {
                    out.push_str("//");
                    out.push_str(comment);
                    cfg.print_break(out, 1, indent)
                } else {
                    out.push_str("/*");
                    out.push_str(comment);
                    out.push_str("*/")
                }
            }
            FmtToken::Sep(n_newlines) => match sep {
                Sep::None => (),
                Sep::Space => out.push(' '),
                Sep::Newline => cfg.print_break(out, *n_newlines, indent),
            },
            FmtToken::Block(block) => block.print(indent, cfg, out),
        };

        if self.tokens.is_empty() {
            if self.spacing.is_some_and(|s| s.before && s.after) {
                out.push(' ');
            }
        } else if let Some(spacing) = self.spacing {
            if spacing.before {
                out.push(' ');
            }
            for token in &self.tokens {
                print_token(token, out, indent, space_if(spacing.between));
            }
            if spacing.after {
                out.push(' ');
            }
        } else {
            let new_indent = indent + cfg.tab_spaces;
            cfg.print_break(out, 1, new_indent);
            for token in &self.tokens {
                print_token(token, out, new_indent, Sep::Newline);
            }
            if let Some(FmtToken::LineComment(_)) = self.tokens.last() {
                out.truncate(out.len() - 4);
            } else {
                cfg.print_break(out, 1, indent);
            }
        }
    }
}

pub struct FmtCtx<'fmt, 'src> {
    pub config: &'fmt Config,
    /// buffer for tokens stored in `FmtBlock`s
    alloc: &'fmt Bump,
    /// for error reporting purposes
    filename: &'src str,
    /// maps line number to byte offset in `input`
    offsets: &'fmt mut StdVec<usize>,
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

impl<'fmt, 'src: 'fmt> Visit<'_> for FmtCtx<'fmt, 'src> {
    fn visit_item(&mut self, i: &'_ Item) {
        let attrs = match i {
            Item::Const(x) => &x.attrs,
            Item::Enum(x) => &x.attrs,
            Item::ExternCrate(x) => &x.attrs,
            Item::Fn(x) => &x.attrs,
            Item::ForeignMod(x) => &x.attrs,
            Item::Impl(x) => &x.attrs,
            Item::Macro(x) => &x.attrs,
            Item::Mod(x) => &x.attrs,
            Item::Static(x) => &x.attrs,
            Item::Struct(x) => &x.attrs,
            Item::Trait(x) => &x.attrs,
            Item::TraitAlias(x) => &x.attrs,
            Item::Type(x) => &x.attrs,
            Item::Union(x) => &x.attrs,
            Item::Use(x) => &x.attrs,
            _ => return,
        };
        if !is_skipped(attrs) {
            syn::visit::visit_item(self, i)
        }
    }

    fn visit_stmt(&mut self, i: &'_ Stmt) {
        let attrs = match i {
            Stmt::Local(x) => &x.attrs,
            Stmt::Macro(x) => &x.attrs,
            Stmt::Item(i) => return syn::visit::visit_item(self, i),
            Stmt::Expr(e, _) => return syn::visit::visit_expr(self, e),
        };
        if !is_skipped(attrs) {
            syn::visit::visit_stmt(self, i);
        }
    }

    // TODO: rewrite with a `try` block when those get stabilised
    fn visit_macro(&mut self, i: &Macro) {
        self.err = (|| -> Result<Option<Diagnostic<()>>> {
            let Some(name) = i.path.segments.last() else {
                return Ok(None);
            };
            if name.ident != "html" && name.ident != "html_nested" {
                return Ok(None);
            }

            let (opening, closing, root_spacing) = match i.delimiter {
                MacroDelimiter::Paren(_) | MacroDelimiter::Bracket(_) => ("(", ")", default()),
                MacroDelimiter::Brace(_) => ("{", "}", Spacing::AROUND),
            };
            let span = i.delimiter.span();
            let (opening_span, closing_span) = (span.open(), span.close());
            let (html_start, html_end) = (opening_span.end(), closing_span.start());
            self.print_source(opening_span.start())?;

            self.print_text(opening, html_start)?;
            if i.tokens.is_empty() {
                self.print_source(html_end)?;
            } else {
                let mut block = FmtBlock::new(
                    self.alloc,
                    Some(root_spacing),
                    ChainingRule::Off,
                    self.pos_to_byte_offset(html_start)?,
                );

                let html = match self.config.yew.html_flavor.parse_root(i.tokens.clone()) {
                    Ok(html) => html,
                    Err(e) => {
                        let span = e.span();
                        let start = self.pos_to_byte_offset(span.start())?;
                        let end = self.pos_to_byte_offset(span.end())?;
                        return Ok(Some(
                            Diagnostic::error()
                                .with_message(e.to_string())
                                .with_labels(vec![Label::primary((), start..end)]),
                        ));
                    }
                };
                html.format(&mut block, self)?;

                self.print_fmt_block(block, html_end)?;
            }
            self.print_text(closing, closing_span.end())?;

            Ok(None)
        })();
    }
}

impl Formatter {
    pub fn new(config: Config) -> Self {
        Self { config, tokens_buf: Bump::new(), offsets: vec![], output: String::new() }
    }

    pub fn format<'fmt, 'src: 'fmt>(
        &'fmt mut self,
        filename: &'src str,
        input: &'src str,
    ) -> Result<FormatResult<'fmt, 'src>> {
        self.output.clear();
        self.offsets.clear();
        self.tokens_buf.reset();
        let mut ctx = FmtCtx {
            alloc: &self.tokens_buf,
            config: &self.config,
            offsets: &mut self.offsets,
            output: &mut self.output,
            filename,
            input,
            err: Ok(None),
            cur_offset: 0,
            cur_pos: LineColumn { line: 1, column: 0 },
        };
        let file = syn::parse_file(input)?;
        ctx.offsets.push(0);
        ctx.offsets.extend(input.char_indices().filter_map(|(i, c)| (c == '\n').then_some(i + 1)));

        ctx.visit_file(&file);
        ctx.finalise()
    }
}

impl<'fmt, 'src> FmtCtx<'fmt, 'src> {
    fn pos_to_byte_offset(&self, LineColumn { line, column }: LineColumn) -> Result<usize> {
        let line_start = *self
            .offsets
            .get(line.saturating_sub(1))
            .with_context(|| format!("line {line} doesn't exist in the source file"))?;
        let column = self.input[line_start..].chars().take(column).map(char::len_utf8).sum();

        line_start.checked_add(column).with_context(|| {
            format!("source position {line}:{column} can't be converted to a byte offset")
        })
    }

    pub fn source_code(&self, loc: Location) -> Result<&'src str> {
        let start =
            self.pos_to_byte_offset(loc.start).context("failed to find the start of the span")?;
        let end = self.pos_to_byte_offset(loc.end).context("failed to find the end of the span")?;
        self.input
            .get(start..end)
            .with_context(|| format!("byte range {start}..{end} is invalid for the source code"))
    }

    fn line_indent(&self, line: usize) -> Result<usize> {
        enum State {
            Space,
            CommentStart,
            Comment,
            CommentEnd,
        }
        let mut state = State::Space;

        let &start = self
            .offsets
            .get(line - 1)
            .with_context(|| format!("line {line} doesn't exist in the source file"))?;
        let line = unsafe { self.input.get_unchecked(start..) };
        for (off, ch) in line.char_visual_offsets(self.config.tab_spaces) {
            match ch {
                ' ' | '\t' => {
                    state = match state {
                        State::Space => continue,
                        State::CommentStart => State::Space,
                        State::Comment => continue,
                        State::CommentEnd => State::Space,
                    }
                }
                '/' => {
                    state = match state {
                        State::Space => State::CommentStart,
                        State::CommentStart => bail!("line {line} of the source file is empty"),
                        State::Comment => continue,
                        State::CommentEnd => State::Space,
                    }
                }
                '*' => {
                    state = match state {
                        State::Space => continue,
                        State::CommentStart => State::Comment,
                        State::Comment => State::CommentEnd,
                        State::CommentEnd => continue,
                    }
                }
                '\n' => bail!("line {line} of the source file is empty"),
                _ => match state {
                    State::Space => return Ok(off),
                    State::CommentStart => return Ok(off - 1),
                    State::Comment => continue,
                    State::CommentEnd => continue,
                },
            }
        }
        bail!("line {line} of the source file is empty")
    }

    fn print_source(&mut self, until: LineColumn) -> Result {
        let until_byte = self.pos_to_byte_offset(until)?;
        let from = self.cur_offset;
        let new = self.input.get(from..until_byte).with_context(|| {
            format!("range {from} .. {until_byte} is out of bounds for the source file")
        })?;
        self.cur_offset = until_byte;
        self.cur_pos = until;
        self.output.push_str(new);
        Ok(())
    }

    // `end` is the position in the source file asssumed to be the end of the text
    fn print_text(&mut self, text: &str, end: LineColumn) -> Result {
        self.output.push_str(text);
        self.cur_pos = end;
        let off = self.pos_to_byte_offset(end)?;
        self.cur_offset = off;
        Ok(())
    }

    // `end` is the position in the source file asssumed to be the end of the formatted sequence
    fn print_fmt_block(&mut self, mut block: FmtBlock<'fmt, 'src>, end: LineColumn) -> Result {
        let indent = self.line_indent(self.cur_pos.line)?;
        block.determine_breaking(self, self.cur_pos.column - indent, indent);
        block.print(indent, self.config, self.output);
        self.cur_pos = end;
        let off = self.pos_to_byte_offset(end)?;
        self.cur_offset = off;
        Ok(())
    }

    fn finalise(self) -> Result<FormatResult<'fmt, 'src>> {
        let rest = unsafe { self.input.get_unchecked(self.cur_offset..) };
        self.output.push_str(rest);
        let new_len = self.output.trim_end().len();
        self.output.truncate(new_len);
        self.output.push('\n');
        self.err.map(|diagnostic| FormatResult {
            filename: self.filename,
            source: self.input,
            output: match diagnostic {
                Some(diagnostic) => Err(diagnostic),
                None => Ok(self.output.as_str()),
            },
        })
    }
}

pub struct FormatResult<'fmt, 'src> {
    filename: &'src str,
    source: &'src str,
    output: Result<&'fmt str, Diagnostic<()>>,
}

impl<'fmt> FormatResult<'fmt, '_> {
    /// if the result is an error, write it into stderr, if it's successfully formatted code,
    /// return it
    pub fn emit_error(self, writer: &mut dyn WriteColor) -> Result<Option<&'fmt str>> {
        let diagnostic = match self.output {
            Ok(out) => return Ok(Some(out)),
            Err(x) => x,
        };
        term::emit(
            writer,
            &term::Config::default(),
            &SimpleFile::new(self.filename, self.source),
            &diagnostic,
        )?;
        Ok(None)
    }
}
