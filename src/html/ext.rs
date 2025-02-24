use crate::{
    config::UseSmallHeuristics,
    formatter::{ChainingRule, FmtBlock, FmtCtx, Format, Located, Location, Spacing},
    html::{
        base::{
            block_children_spacing, format_children, parse_children, HtmlBlock, HtmlBlockContent,
            HtmlElement, HtmlIf,
        },
        HtmlFlavorSpec,
    },
    utils::{default, OptionExt, Result, TokenIter, TokenTreeExt},
};
use proc_macro2::LineColumn;
use syn::{
    braced,
    buffer::Cursor,
    parse::{Parse, ParseStream},
    punctuated::{Pair, Punctuated},
    token::Brace,
    Expr, Local, LocalInit, Pat, PatType, Token,
};

pub struct ExtHtmlFlavor;

impl HtmlFlavorSpec for ExtHtmlFlavor {
    type Root = Html;
    type Tree = HtmlTree;

    fn element_children_spacing(ctx: &FmtCtx, children: &[Self::Tree]) -> Option<Spacing> {
        match ctx.config.yew.use_small_heuristics {
            UseSmallHeuristics::Off => None,
            UseSmallHeuristics::Default => {
                children.iter().all(|child| matches!(child, HtmlTree::Block(_))).then(default)
            }
            UseSmallHeuristics::Max => Some(default()),
        }
    }
}

pub enum Html {
    Tree(Vec<HtmlTree>),
    Value(Box<HtmlBlockContent>),
}

pub enum HtmlTreeKind {
    Element,
    Block,
    If,
    For,
    Match,
}

pub enum HtmlTree {
    Element(Box<HtmlElement<ExtHtmlFlavor>>),
    Block(Box<HtmlBlock>),
    If(Box<HtmlIf<ExtHtmlFlavor>>),
    For(Box<HtmlFor>),
    Match(Box<HtmlMatch>),
    Let(Box<HtmlLet>),
}

pub struct HtmlFor {
    pub for_token: Token![for],
    pub pat: Pat,
    pub in_token: Token![in],
    pub iter: Expr,
    pub brace: Brace,
    pub body: Vec<HtmlTree>,
}

pub struct HtmlMatch {
    pub match_token: Token![match],
    pub expr: Expr,
    pub brace: Brace,
    pub arms: Punctuated<HtmlMatchArm, Token![,]>,
}

pub struct HtmlMatchArm {
    pub pat: Pat,
    pub guard: Option<(Token![if], Box<Expr>)>,
    pub fat_arrow_token: Token![=>],
    pub body: HtmlTree,
}

pub struct HtmlLet(Local);

impl Parse for Html {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        /// Assumes that `cursor` is pointing at a `for` token
        fn for_loop_like(cursor: Cursor<'_>) -> bool {
            let Some((_, cursor)) = cursor.token_tree() else { return false };
            // The `take_while` call makes sure that e.g. `html!(for for i in 0 .. 10 {})` is
            // classified correctly
            TokenIter(cursor).take_while(|t| !t.is_ident("for")).any(|t| t.is_ident("in"))
        }

        let mut nodes = vec![];
        while !input.is_empty() {
            let cursor = input.cursor();
            nodes.push(match HtmlTree::peek_html_type(cursor) {
                Some(HtmlTreeKind::For) => {
                    if for_loop_like(cursor) {
                        HtmlTree::For(input.parse()?)
                    } else {
                        return Ok(Self::Value(
                            HtmlBlockContent::Iterable(input.parse()?, input.parse()?).into(),
                        ));
                    }
                }
                Some(HtmlTreeKind::Match) => HtmlTree::Match(input.parse()?),
                Some(HtmlTreeKind::If) => HtmlTree::If(input.parse()?),
                Some(HtmlTreeKind::Block) => HtmlTree::Block(input.parse()?),
                Some(HtmlTreeKind::Element) => HtmlTree::Element(input.parse()?),
                None => return Ok(Self::Value(input.parse()?)),
            })
        }

        Ok(Self::Tree(nodes))
    }
}

impl Parse for HtmlTree {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let cursor = input.cursor();
        Ok(if HtmlIf::<ExtHtmlFlavor>::parseable(cursor) {
            Self::If(input.parse()?)
        } else if HtmlElement::<ExtHtmlFlavor>::parseable(cursor) {
            Self::Element(input.parse()?)
        } else if HtmlFor::parseable(cursor) {
            Self::For(input.parse()?)
        } else if HtmlMatch::parseable(cursor) {
            Self::Match(input.parse()?)
        } else if HtmlLet::parseable(cursor) {
            Self::Let(input.parse()?)
        } else {
            Self::Block(input.parse()?)
        })
    }
}

impl Parse for HtmlFor {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let body;
        Ok(Self {
            for_token: input.parse()?,
            pat: Pat::parse_multi_with_leading_vert(input)?,
            in_token: input.parse()?,
            iter: Expr::parse_without_eager_brace(input)?,
            brace: braced!(body in input),
            body: parse_children(&body)?,
        })
    }
}

impl Parse for HtmlMatch {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let body;
        Ok(Self {
            match_token: input.parse()?,
            expr: Expr::parse_without_eager_brace(input)?,
            brace: braced!(body in input),
            arms: Punctuated::parse_terminated(&body)?,
        })
    }
}

impl Parse for HtmlMatchArm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            pat: Pat::parse_multi_with_leading_vert(input)?,
            guard: if let Ok(if_token) = input.parse() {
                Some((if_token, input.parse()?))
            } else {
                None
            },
            fat_arrow_token: input.parse()?,
            body: input.parse()?,
        })
    }
}

impl Parse for HtmlLet {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let let_token = input.parse()?;

        let mut pat = Pat::parse_single(input)?;
        if let Some(colon_token) = input.parse()? {
            pat = Pat::Type(PatType {
                attrs: vec![],
                pat: Box::new(pat),
                colon_token,
                ty: input.parse()?,
            });
        }

        let init = if let Some(eq_token) = input.parse()? {
            Some(LocalInit {
                eq_token,
                expr: input.parse()?,
                diverge: Option::<Token![else]>::parse(input)?.try_zip(|| input.parse())?,
            })
        } else {
            None
        };

        Ok(Self(Local { attrs: vec![], let_token, pat, init, semi_token: input.parse()? }))
    }
}

impl HtmlTree {
    fn peek_html_type(cursor: Cursor) -> Option<HtmlTreeKind> {
        if HtmlIf::<ExtHtmlFlavor>::parseable(cursor) {
            Some(HtmlTreeKind::If)
        } else if HtmlFor::parseable(cursor) {
            Some(HtmlTreeKind::For)
        } else if HtmlMatch::parseable(cursor) {
            Some(HtmlTreeKind::Match)
        } else if HtmlElement::<ExtHtmlFlavor>::parseable(cursor) {
            Some(HtmlTreeKind::Element)
        } else if HtmlBlock::parseable(cursor) {
            Some(HtmlTreeKind::Block)
        } else {
            None
        }
    }
}

impl HtmlFor {
    fn parseable(cursor: Cursor) -> bool {
        cursor.ident().is_some_and(|(i, _)| i == "for")
    }
}

impl HtmlMatch {
    fn parseable(cursor: Cursor) -> bool {
        cursor.ident().is_some_and(|(i, _)| i == "match")
    }
}

impl HtmlLet {
    fn parseable(cursor: Cursor) -> bool {
        cursor.ident().is_some_and(|(i, _)| i == "let")
    }
}

impl Format for Html {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        match self {
            Self::Tree(trees) => {
                let [Some(first), Some(last)] = [trees.first(), trees.last()] else {
                    return Ok(());
                };
                format_children::<ExtHtmlFlavor>(
                    block,
                    ctx,
                    Location::zero_width(first.start()),
                    Location::zero_width(last.end()),
                    false,
                    trees,
                )?;
                block.flatten();
                Ok(())
            }
            Self::Value(val) => val.format(block, ctx),
        }
    }
}

impl Format for HtmlTree {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        match self {
            HtmlTree::Element(e) => e.format(block, ctx),
            HtmlTree::Block(b) => b.format(block, ctx),
            HtmlTree::If(i) => i.format(block, ctx),
            HtmlTree::For(f) => f.format(block, ctx),
            HtmlTree::Match(m) => m.format(block, ctx),
            HtmlTree::Let(l) => l.format(block, ctx),
        }
    }
}

impl Format for HtmlFor {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        block.add_source(ctx, self.for_token)?;
        block.add_source_with_space(ctx, &self.pat)?;
        block.add_source_with_space(ctx, self.in_token)?;
        block.add_source_with_space(ctx, &self.iter)?;
        block.add_delimited_block_with_space(
            ctx,
            self.brace.span.open(),
            self.brace.span.close(),
            block_children_spacing(ctx),
            ChainingRule::Off,
            |block, ctx| {
                for child in &self.body {
                    child.format(block, ctx)?;
                    block.add_sep(ctx, child.end())?;
                }
                Ok(())
            },
        )
    }
}

impl Format for HtmlMatch {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        block.add_source(ctx, self.match_token)?;
        block.add_source_with_space(ctx, &self.expr)?;
        block.add_delimited_block_with_space(
            ctx,
            self.brace.span.open(),
            self.brace.span.close(),
            block_children_spacing(ctx).map(|s| Spacing { between: true, ..s }),
            ChainingRule::Off,
            |block, ctx| {
                for (arm, comma) in self.arms.pairs().map(Pair::into_tuple) {
                    arm.format(block, ctx)?;
                    let sep_at = if let Some(comma) = comma {
                        block.add_source(ctx, comma)?;
                        comma.end()
                    } else {
                        let at = arm.end();
                        block.add_text(ctx, ",", at)?;
                        LineColumn { line: at.line, column: at.column + 1 }
                    };
                    block.add_aware_sep(ctx, sep_at, 1, 2)?;
                }
                Ok(())
            },
        )
    }
}

impl Format for HtmlMatchArm {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        block.add_source(ctx, &self.pat)?;
        if let Some((if_token, guard)) = &self.guard {
            block.add_source_with_space(ctx, if_token)?;
            block.add_source_with_space(ctx, guard)?;
        }
        block.add_source_with_space(ctx, self.fat_arrow_token)?;
        self.body.format_with_space(block, ctx)
    }
}

impl Format for HtmlLet {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        let Self(stmt) = self;
        block.add_source(ctx, stmt.let_token)?;
        if let Pat::Type(PatType { pat, colon_token, ty, .. }) = &stmt.pat {
            block.add_source_with_space(ctx, pat)?;
            block.add_source_with_space(ctx, colon_token)?;
            block.add_source_with_space(ctx, ty)?;
        } else {
            block.add_source_with_space(ctx, &stmt.pat)?;
        };
        if let Some(init) = &stmt.init {
            block.add_source_with_space(ctx, init.eq_token)?;
            block.add_source_with_space(ctx, &init.expr)?;
            if let Some((else_token, expr)) = &init.diverge {
                block.add_source_with_space(ctx, else_token)?;
                block.add_source_with_space(ctx, expr)?;
            }
        }
        block.add_source(ctx, stmt.semi_token)
    }
}

impl Located for Html {
    fn start(&self) -> LineColumn {
        match self {
            Self::Tree(tree) => {
                tree.first().map_or(LineColumn { line: 1, column: 0 }, Located::start)
            }
            Self::Value(val) => val.start(),
        }
    }

    fn end(&self) -> LineColumn {
        match self {
            Self::Tree(tree) => tree.last().map_or(LineColumn { line: 1, column: 0 }, Located::end),
            Self::Value(val) => val.end(),
        }
    }

    fn loc(&self) -> Location {
        match self {
            Self::Tree(tree) => Location {
                start: tree.first().map_or(LineColumn { line: 1, column: 0 }, Located::start),
                end: tree.last().map_or(LineColumn { line: 1, column: 0 }, Located::end),
            },
            Self::Value(val) => val.loc(),
        }
    }
}

impl Located for HtmlTree {
    fn start(&self) -> LineColumn {
        match self {
            Self::Element(x) => x.start(),
            Self::Block(x) => x.start(),
            Self::If(x) => x.start(),
            Self::For(x) => x.start(),
            Self::Match(x) => x.start(),
            Self::Let(x) => x.start(),
        }
    }

    fn end(&self) -> LineColumn {
        match self {
            Self::Element(x) => x.end(),
            Self::Block(x) => x.end(),
            Self::If(x) => x.end(),
            Self::For(x) => x.end(),
            Self::Match(x) => x.end(),
            Self::Let(x) => x.end(),
        }
    }

    fn loc(&self) -> Location {
        match self {
            Self::Element(x) => x.loc(),
            Self::Block(x) => x.loc(),
            Self::If(x) => x.loc(),
            Self::For(x) => x.loc(),
            Self::Match(x) => x.loc(),
            Self::Let(x) => x.loc(),
        }
    }
}

impl Located for HtmlFor {
    fn start(&self) -> LineColumn {
        self.for_token.span.start()
    }

    fn end(&self) -> LineColumn {
        self.brace.span.end()
    }
}

impl Located for HtmlMatch {
    fn start(&self) -> LineColumn {
        self.match_token.start()
    }

    fn end(&self) -> LineColumn {
        self.brace.span.end()
    }
}

impl Located for HtmlMatchArm {
    fn start(&self) -> LineColumn {
        self.pat.start()
    }

    fn end(&self) -> LineColumn {
        self.body.end()
    }
}

impl Located for HtmlLet {
    fn start(&self) -> LineColumn {
        self.0.let_token.start()
    }

    fn end(&self) -> LineColumn {
        self.0.semi_token.end()
    }
}
