use {
    crate::{
        config::UseSmallHeuristics,
        css::StyleString,
        formatter::{ChainingRule, FmtBlock, FmtCtx, Format, Located, Location, Spacing},
        html::HtmlFlavorSpec,
        utils::{default, AnyIdent, OptionExt, Result},
    },
    anyhow::Context,
    proc_macro2::{Delimiter, LineColumn, TokenStream, TokenTree},
    quote::ToTokens,
    std::{iter::from_fn, ops::Not},
    syn::{
        braced,
        buffer::Cursor,
        parse::{Parse, ParseStream},
        parse2,
        punctuated::Punctuated,
        spanned::Spanned,
        token::Brace,
        Block, Expr, Lit, Stmt, Token, Type,
    },
};

pub fn parse_children<T: Parse>(input: ParseStream) -> syn::Result<Vec<T>> {
    let mut res = vec![];
    while !(input.is_empty() || input.peek(Token![<]) && input.peek2(Token![/])) {
        res.push(input.parse()?)
    }
    Ok(res)
}

pub fn format_children<'src, F: HtmlFlavorSpec>(
    block: &mut FmtBlock<'_, 'src>,
    ctx: &mut FmtCtx<'_, 'src>,
    opening: impl Located,
    closing: impl Located,
    had_props_block: bool,
    children: &[F::Tree],
) -> Result {
    block.add_delimited_block(
        ctx,
        opening,
        closing,
        F::element_children_spacing(ctx, children),
        if had_props_block { ChainingRule::End } else { ChainingRule::Off },
        |block, ctx| {
            for child in children {
                child.format(block, ctx)?;
                block.add_sep(ctx, child.end())?;
            }
            Ok(())
        },
    )
}

pub struct BaseHtmlFlavor;

impl HtmlFlavorSpec for BaseHtmlFlavor {
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
    Tree(HtmlTree),
    Value(Box<HtmlBlockContent>),
}

pub enum HtmlTreeKind {
    Element,
    Block,
    If,
}

pub enum HtmlTree {
    Element(Box<HtmlElement<BaseHtmlFlavor>>),
    Block(Box<HtmlBlock>),
    If(Box<HtmlIf<BaseHtmlFlavor>>),
}

pub struct HtmlBlock {
    pub brace: Brace,
    pub content: Option<HtmlBlockContent>,
}

pub enum HtmlBlockContent {
    Expr(Expr),
    Iterable(Token![for], Expr),
}

pub enum HtmlElement<F: HtmlFlavorSpec> {
    Fragment(HtmlFragment<F>),
    Dynamic(HtmlDynamicElement<F>),
    Literal(HtmlLiteralElement<F>),
}

pub struct HtmlFragment<F: HtmlFlavorSpec> {
    pub lt_token: Token![<],
    pub key: Option<HtmlProp>,
    pub gt_token: Token![>],
    pub children: Vec<F::Tree>,
    pub closing_lt_token: Token![<],
    pub div_token: Token![/],
    pub closing_gt_token: Token![>],
}

pub struct HtmlDynamicElement<F: HtmlFlavorSpec> {
    pub lt_token: Token![<],
    pub at_token: Token![@],
    pub name: Block,
    pub props: Vec<HtmlProp>,
    pub children: Vec<F::Tree>,
    pub closing_tag: Option<(Token![>], Token![<], Token![@])>,
    pub div_token: Token![/],
    pub closing_gt_token: Token![>],
}

pub struct HtmlLiteralElement<F: HtmlFlavorSpec> {
    pub lt_token: Token![<],
    pub name: TokenStream,
    pub props: Vec<HtmlProp>,
    pub prop_base: Option<Box<(Token![..], Expr)>>,
    pub children: Vec<F::Tree>,
    pub closing_tag: Option<(Token![>], Token![<], TokenStream)>,
    pub div_token: Token![/],
    pub closing_gt_token: Token![>],
}

pub struct HtmlProp {
    pub access_spec: Option<Token![~]>,
    pub kind: HtmlPropKind,
}

pub enum HtmlPropKind {
    Shortcut(Brace, Punctuated<AnyIdent, Token![-]>),
    Literal(Punctuated<AnyIdent, Token![-]>, Token![=], Lit),
    Block(Punctuated<AnyIdent, Token![-]>, Token![=], Block),
    Style(Punctuated<AnyIdent, Token![-]>, Token![=], StyleString),
}

pub struct HtmlIf<F: HtmlFlavorSpec> {
    pub if_token: Token![if],
    pub condition: Expr,
    pub brace: Brace,
    pub then_branch: Vec<F::Tree>,
    pub else_branch: Option<HtmlElse<F>>,
}

pub enum HtmlElse<F: HtmlFlavorSpec> {
    If(Token![else], Box<HtmlIf<F>>),
    Tree(Token![else], Brace, Vec<F::Tree>),
}

impl Parse for Html {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(match HtmlTree::peek_html_type(input.cursor()) {
            Some(HtmlTreeKind::If) => Self::Tree(HtmlTree::If(input.parse()?)),
            Some(HtmlTreeKind::Block) => Self::Tree(HtmlTree::Block(input.parse()?)),
            Some(HtmlTreeKind::Element) => Self::Tree(HtmlTree::Element(input.parse()?)),
            None => Self::Value(input.parse()?),
        })
    }
}

impl Parse for HtmlTree {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let cursor = input.cursor();
        if HtmlIf::<BaseHtmlFlavor>::parseable(cursor) {
            input.parse().map(Self::If)
        } else if HtmlElement::<BaseHtmlFlavor>::parseable(cursor) {
            input.parse().map(Self::Element)
        } else {
            input.parse().map(Self::Block)
        }
    }
}

impl HtmlBlock {
    pub fn parseable(cursor: Cursor) -> bool {
        cursor.group(Delimiter::Brace).is_some()
    }
}

impl Parse for HtmlBlock {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        let brace = braced!(content in input);
        content
            .is_empty()
            .not()
            .then(|| HtmlBlockContent::parse(&content))
            .transpose()
            .map(|content| Self { content, brace })
    }
}

impl Parse for HtmlBlockContent {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Ok(for_token) = input.parse() {
            Ok(Self::Iterable(for_token, input.parse()?))
        } else {
            input.parse().map(Self::Expr)
        }
    }
}

impl<F: HtmlFlavorSpec> HtmlElement<F> {
    pub fn parseable(cursor: Cursor) -> bool {
        cursor.punct().is_some_and(|(p, _)| p.as_char() == '<')
    }
}

impl<F: HtmlFlavorSpec> Parse for HtmlElement<F> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if HtmlFragment::<F>::parseable(input.cursor()) {
            input.parse().map(Self::Fragment)
        } else if HtmlDynamicElement::<F>::parseable(input.cursor()) {
            input.parse().map(Self::Dynamic)
        } else {
            input.parse().map(Self::Literal)
        }
    }
}

impl<F: HtmlFlavorSpec> HtmlFragment<F> {
    fn parseable(cursor: Cursor) -> bool {
        let Some((_, cursor)) = cursor.punct().filter(|(p, _)| p.as_char() == '<') else {
            return false;
        };
        match cursor.token_tree() {
            Some((TokenTree::Punct(p), _)) if p.as_char() == '>' => true, // <>...
            Some((_, c)) => c.punct().is_some_and(|(p, _)| p.as_char() == '='), // <key=...
            None => false,
        }
    }
}

impl<F: HtmlFlavorSpec> Parse for HtmlFragment<F> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lt_token = input.parse()?;
        let (key, gt_token) = if input.peek(Token![>]) {
            (None, input.parse()?)
        } else {
            (Some(input.parse()?), input.parse()?)
        };

        Ok(Self {
            lt_token,
            key,
            gt_token,
            children: parse_children(input)?,
            closing_lt_token: input.parse()?,
            div_token: input.parse()?,
            closing_gt_token: input.parse()?,
        })
    }
}

impl<F: HtmlFlavorSpec> Parse for HtmlDynamicElement<F> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lt_token = input.parse()?;
        let at_token = input.parse()?;
        let name = input.parse()?;

        let mut props = vec![];
        while input.cursor().punct().is_none() {
            props.push(input.parse()?)
        }

        let (children, closing_tag, div_token) = if input.peek(Token![>]) {
            let gt_token = input.parse()?;
            let children = parse_children(input)?;
            let closing_lt_token = input.parse()?;
            let div_token = input.parse()?;
            let closing_at_token = input.parse()?;
            (children, Some((gt_token, closing_lt_token, closing_at_token)), div_token)
        } else {
            (vec![], None, input.parse()?)
        };

        Ok(Self {
            lt_token,
            at_token,
            name,
            props,
            children,
            closing_tag,
            div_token,
            closing_gt_token: input.parse()?,
        })
    }
}

impl<F: HtmlFlavorSpec> Parse for HtmlLiteralElement<F> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        fn prop_base_collector(input: ParseStream) -> impl Iterator<Item = TokenTree> + '_ {
            from_fn(move || {
                (!input.peek(Token![>]) && !input.peek(Token![/])).then(|| input.parse().ok())?
            })
        }

        fn get_name(input: ParseStream) -> syn::Result<TokenStream> {
            if let Ok(ty) = Type::parse(input) {
                Ok(ty.into_token_stream())
            } else {
                Punctuated::<AnyIdent, Token![-]>::parse_separated_nonempty(input)
                    .map(ToTokens::into_token_stream)
            }
        }

        let lt_token = input.parse()?;
        let name = get_name(input)?;

        let mut props = vec![];
        while input.cursor().punct().is_none() {
            props.push(input.parse()?)
        }
        let prop_base = if input.peek(Token![..]) {
            Some((input.parse()?, parse2(prop_base_collector(input).collect())?).into())
        } else {
            None
        };

        let (children, closing_tag, div_token) = if input.peek(Token![>]) {
            let gt_token = input.parse()?;
            let children = parse_children(input)?;
            let closing_lt_token = input.parse()?;
            let div_token = input.parse()?;
            let closing_name = get_name(input)?;
            (children, Some((gt_token, closing_lt_token, closing_name)), div_token)
        } else {
            (vec![], None, input.parse()?)
        };

        Ok(Self {
            lt_token,
            name,
            props,
            prop_base,
            children,
            closing_tag,
            div_token,
            closing_gt_token: input.parse()?,
        })
    }
}

impl Parse for HtmlProp {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let access_spec = input.parse().ok();
        let kind = if input.peek(Brace) {
            let inner;
            let brace = braced!(inner in input);
            HtmlPropKind::Shortcut(brace, Punctuated::parse_terminated(&inner)?)
        } else {
            let name = Punctuated::parse_separated_nonempty(input)?;
            let eq_token = input.parse()?;
            if input.peek(Brace) {
                HtmlPropKind::Block(name, eq_token, input.parse()?)
            } else {
                HtmlPropKind::Literal(name, eq_token, input.parse()?)
            }
        };
        Ok(Self { access_spec, kind })
    }
}

impl<F: HtmlFlavorSpec> Parse for HtmlIf<F> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let then_block;
        Ok(Self {
            if_token: input.parse()?,
            condition: Expr::parse_without_eager_brace(input)?,
            brace: braced!(then_block in input),
            then_branch: parse_children(&then_block)?,
            else_branch: HtmlElse::<F>::parseable(input.cursor())
                .then(|| input.parse())
                .transpose()?,
        })
    }
}

impl<F: HtmlFlavorSpec> Parse for HtmlElse<F> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let else_token = input.parse()?;
        Ok(if HtmlIf::<F>::parseable(input.cursor()) {
            Self::If(else_token, input.parse()?)
        } else {
            let inner;
            Self::Tree(else_token, braced!(inner in input), parse_children(&inner)?)
        })
    }
}

impl HtmlTree {
    fn peek_html_type(cursor: Cursor) -> Option<HtmlTreeKind> {
        if HtmlIf::<BaseHtmlFlavor>::parseable(cursor) {
            Some(HtmlTreeKind::If)
        } else if HtmlElement::<BaseHtmlFlavor>::parseable(cursor) {
            Some(HtmlTreeKind::Element)
        } else if HtmlBlock::parseable(cursor) {
            Some(HtmlTreeKind::Block)
        } else {
            None
        }
    }
}

impl<F: HtmlFlavorSpec> HtmlDynamicElement<F> {
    fn parseable(cursor: Cursor) -> bool {
        let Some((_, cursor)) = cursor.punct().filter(|(p, _)| p.as_char() == '<') else {
            return false;
        };
        cursor.punct().is_some_and(|(p, _)| p.as_char() == '@')
    }
}

impl<F: HtmlFlavorSpec> HtmlIf<F> {
    pub fn parseable(cursor: Cursor) -> bool {
        cursor.ident().is_some_and(|(i, _)| i == "if")
    }
}

impl<F: HtmlFlavorSpec> HtmlElse<F> {
    fn parseable(cursor: Cursor) -> bool {
        cursor.ident().is_some_and(|(i, _)| i == "else")
    }
}

pub const fn props_spacing(self_closing: bool) -> Spacing {
    Spacing { before: true, between: true, after: self_closing }
}

pub fn block_children_spacing(ctx: &FmtCtx) -> Option<Spacing> {
    (ctx.config.yew.use_small_heuristics == UseSmallHeuristics::Max).then_some(Spacing::AROUND)
}

impl Format for Html {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        match self {
            Html::Tree(tree) => tree.format(block, ctx),
            Html::Value(val) => val.format(block, ctx),
        }
    }
}

impl Format for HtmlTree {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        match self {
            HtmlTree::Element(e) => e.format(block, ctx),
            HtmlTree::Block(b) => b.format(block, ctx),
            HtmlTree::If(i) => i.format(block, ctx),
        }
    }
}

impl<F: HtmlFlavorSpec> Format for HtmlElement<F> {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        match self {
            HtmlElement::Fragment(f) => f.format(block, ctx),
            HtmlElement::Dynamic(d) => d.format(block, ctx),
            HtmlElement::Literal(l) => l.format(block, ctx),
        }
    }
}

impl<F: HtmlFlavorSpec> Format for HtmlFragment<F> {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        block.add_source(ctx, self.lt_token)?;
        if let Some(key) = &self.key {
            key.format(block, ctx)?;
        }

        format_children::<F>(
            block,
            ctx,
            self.gt_token,
            self.closing_lt_token,
            false,
            &self.children,
        )?;

        block.add_source(ctx, self.div_token)?;
        block.add_source(ctx, self.closing_gt_token)
    }
}

impl<F: HtmlFlavorSpec> Format for HtmlDynamicElement<F> {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        block.add_source(ctx, self.lt_token)?;
        block.add_source(ctx, self.at_token)?;
        self.name.format(block, ctx)?;
        let closing_tag = self
            .closing_tag
            .as_ref()
            .filter(|_| !self.children.is_empty() || !ctx.config.yew.self_close_elements);

        block.add_block(Some(props_spacing(closing_tag.is_none())), ChainingRule::On, |block| {
            for prop in &self.props {
                prop.format(block, ctx)?;
                block.add_sep(ctx, prop.end())?;
            }
            anyhow::Ok(())
        })?;

        if let Some((gt, closing_lt, closing_at)) = closing_tag {
            format_children::<F>(block, ctx, gt, closing_lt, true, &self.children)?;
            block.add_source(ctx, self.div_token)?;
            block.add_source(ctx, closing_at)?;
            block.add_source(ctx, self.closing_gt_token)
        } else {
            block.add_source(ctx, self.div_token)?;
            block.add_source(ctx, self.closing_gt_token)
        }
    }
}

impl<F: HtmlFlavorSpec> Format for HtmlLiteralElement<F> {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        block.add_source(ctx, self.lt_token)?;
        block.add_source_spanned_by_iter(ctx, self.name.clone())?;
        let closing_tag = self
            .closing_tag
            .as_ref()
            .filter(|_| !self.children.is_empty() || !ctx.config.yew.self_close_elements);

        block.add_block(
            Some(props_spacing(closing_tag.is_none())),
            closing_tag.choose(ChainingRule::On, ChainingRule::Off),
            |block| {
                for prop in &self.props {
                    prop.format(block, ctx)?;
                    block.add_sep(ctx, prop.end())?;
                }
                if let Some((dotdot, prop_base)) = self.prop_base.as_deref() {
                    block.add_source(ctx, dotdot)?;
                    block.add_source(ctx, prop_base)?;
                    block.add_sep(ctx, prop_base.end())?;
                }
                anyhow::Ok(())
            },
        )?;

        if let Some((gt, closing_lt, closing_name)) = closing_tag {
            format_children::<F>(block, ctx, gt, closing_lt, true, &self.children)?;
            block.add_source(ctx, self.div_token)?;
            block.add_source_spanned_by_iter(ctx, closing_name.clone())?;
            block.add_source(ctx, self.closing_gt_token)
        } else {
            block.add_source(ctx, self.div_token)?;
            block.add_source(ctx, self.closing_gt_token)
        }
    }
}

impl Format for HtmlProp {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        block.maybe_add_source(ctx, self.access_spec)?;
        match &self.kind {
            HtmlPropKind::Shortcut(brace, name) => {
                block.add_source(ctx, brace.span.open())?;
                block.add_source_punctuated(ctx, name)?;
                block.add_source(ctx, brace.span.close())
            }

            HtmlPropKind::Literal(name, eq, lit) => {
                block.add_source_punctuated(ctx, name)?;
                block.add_source(ctx, eq)?;
                block.add_source(ctx, lit)
            }

            HtmlPropKind::Block(name, eq, expr) => match &*expr.stmts {
                [Stmt::Expr(Expr::Path(p), None)]
                    if ctx.config.yew.use_prop_init_shorthand
                        && name.len() == 1
                        && p.path.is_ident(&**name.first().context("prop name is empty")?) =>
                {
                    expr.format(block, ctx)
                }
                [Stmt::Expr(Expr::Lit(l), None)] if ctx.config.yew.unwrap_literal_prop_values => {
                    block.add_source_punctuated(ctx, name)?;
                    block.add_source(ctx, eq)?;
                    block.add_source(ctx, l)
                }
                _ => {
                    block.add_source_punctuated(ctx, name)?;
                    block.add_source(ctx, eq)?;
                    expr.format(block, ctx)
                }
            },

            HtmlPropKind::Style(name, eq, style) => {
                block.add_source_punctuated(ctx, name)?;
                block.add_source(ctx, eq)?;
                style.format(block, ctx)
            }
        }
    }
}

impl Format for Block {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        block.add_source(ctx, self.brace_token.span.open())?;
        match &*self.stmts {
            [] => (),
            [only] => block.add_source(ctx, only)?,
            [first, .., last] => {
                block.add_source(ctx, Location { start: first.start(), end: last.end() })?
            }
        }
        block.add_source(ctx, self.brace_token.span.close())
    }
}

impl Format for HtmlBlock {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        block.add_source(ctx, self.brace.span.open())?;
        let closing_brace_span = self.brace.span.close();
        if let Some(content) = &self.content {
            content.format_with_space(block, ctx)?;
            block.add_space(ctx, closing_brace_span.start())?;
        }
        block.add_source(ctx, closing_brace_span)
    }
}

impl Format for HtmlBlockContent {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        match self {
            Self::Expr(e) => block.add_source(ctx, e),
            Self::Iterable(r#for, e) => {
                block.add_source(ctx, r#for)?;
                block.add_source_with_space(ctx, e)
            }
        }
    }
}

impl<F: HtmlFlavorSpec> Format for HtmlIf<F> {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        block.add_source(ctx, self.if_token.loc())?;
        block.add_source_with_space(ctx, &self.condition)?;
        block.add_delimited_block_with_space(
            ctx,
            self.brace.span.open(),
            self.brace.span.close(),
            block_children_spacing(ctx),
            self.else_branch.choose(ChainingRule::On, ChainingRule::End),
            |block, ctx| {
                for child in &self.then_branch {
                    child.format(block, ctx)?;
                    block.add_sep(ctx, child.end())?;
                }
                Ok(())
            },
        )?;
        self.else_branch.as_ref().try_map_or((), |b| b.format_with_space(block, ctx))
    }
}

impl<F: HtmlFlavorSpec> Format for HtmlElse<F> {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        match self {
            Self::If(r#else, r#if) => {
                block.add_source(ctx, r#else)?;
                r#if.format_with_space(block, ctx)
            }
            Self::Tree(r#else, brace, children) => {
                block.add_source(ctx, r#else)?;
                block.add_delimited_block_with_space(
                    ctx,
                    brace.span.open(),
                    brace.span.close(),
                    block_children_spacing(ctx),
                    ChainingRule::End,
                    |block, ctx| {
                        for child in children {
                            child.format(block, ctx)?;
                            block.add_sep(ctx, child.end())?;
                        }
                        Ok(())
                    },
                )
            }
        }
    }
}

impl Located for Html {
    fn start(&self) -> LineColumn {
        match self {
            Self::Tree(tree) => tree.start(),
            Self::Value(val) => val.start(),
        }
    }

    fn end(&self) -> LineColumn {
        match self {
            Self::Tree(tree) => tree.end(),
            Self::Value(val) => val.end(),
        }
    }

    fn loc(&self) -> Location {
        match self {
            Self::Tree(tree) => tree.loc(),
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
        }
    }

    fn end(&self) -> LineColumn {
        match self {
            Self::Element(x) => x.end(),
            Self::Block(x) => x.end(),
            Self::If(x) => x.end(),
        }
    }

    fn loc(&self) -> Location {
        match self {
            Self::Element(x) => x.loc(),
            Self::Block(x) => x.loc(),
            Self::If(x) => x.loc(),
        }
    }
}

impl<F: HtmlFlavorSpec> Located for HtmlElement<F> {
    fn start(&self) -> LineColumn {
        match self {
            Self::Fragment(x) => x.start(),
            Self::Dynamic(x) => x.start(),
            Self::Literal(x) => x.start(),
        }
    }

    fn end(&self) -> LineColumn {
        match self {
            Self::Fragment(x) => x.end(),
            Self::Dynamic(x) => x.end(),
            Self::Literal(x) => x.end(),
        }
    }

    fn loc(&self) -> Location {
        match self {
            Self::Fragment(x) => x.loc(),
            Self::Dynamic(x) => x.loc(),
            Self::Literal(x) => x.loc(),
        }
    }
}

impl<F: HtmlFlavorSpec> Located for HtmlFragment<F> {
    fn start(&self) -> LineColumn {
        self.lt_token.start()
    }
    fn end(&self) -> LineColumn {
        self.closing_gt_token.end()
    }
}

impl<F: HtmlFlavorSpec> Located for HtmlDynamicElement<F> {
    fn start(&self) -> LineColumn {
        self.lt_token.start()
    }

    fn end(&self) -> LineColumn {
        self.closing_gt_token.end()
    }
}

impl<F: HtmlFlavorSpec> Located for HtmlLiteralElement<F> {
    fn start(&self) -> LineColumn {
        self.lt_token.start()
    }

    fn end(&self) -> LineColumn {
        self.closing_gt_token.end()
    }
}

impl Located for HtmlProp {
    fn start(&self) -> LineColumn {
        if let Some(tilde) = &self.access_spec {
            return tilde.span.start();
        }

        match &self.kind {
            HtmlPropKind::Shortcut(brace, _) => brace.span.open().start(),
            HtmlPropKind::Literal(name, ..)
            | HtmlPropKind::Block(name, ..)
            | HtmlPropKind::Style(name, ..) => unsafe {
                // Safety: the name of the prop is guaranteed to be non-empty by
                // `Punctuated::parse_terminated(_nonempty)`
                name.first().unwrap_unchecked().span().start()
            },
        }
    }

    fn end(&self) -> LineColumn {
        match &self.kind {
            HtmlPropKind::Shortcut(brace, _) => brace.span.close().end(),
            HtmlPropKind::Literal(.., lit) => lit.span().end(),
            HtmlPropKind::Block(.., expr) => expr.brace_token.span.close().end(),
            HtmlPropKind::Style(.., style) => style.end(),
        }
    }
}

impl Located for HtmlBlock {
    fn start(&self) -> LineColumn {
        self.brace.span.start()
    }

    fn end(&self) -> LineColumn {
        self.brace.span.end()
    }
}

impl Located for HtmlBlockContent {
    fn start(&self) -> LineColumn {
        match self {
            Self::Expr(expr) => expr.span().start(),
            Self::Iterable(r#for, _) => r#for.span.start(),
        }
    }

    fn end(&self) -> LineColumn {
        match self {
            Self::Expr(expr) => expr,
            Self::Iterable(_, expr) => expr,
        }
        .span()
        .end()
    }

    fn loc(&self) -> Location {
        match self {
            Self::Expr(expr) => expr.loc(),
            Self::Iterable(r#for, expr) => Location { start: r#for.span.start(), end: expr.end() },
        }
    }
}

impl<F: HtmlFlavorSpec> Located for HtmlIf<F> {
    fn start(&self) -> LineColumn {
        self.if_token.span.start()
    }

    fn end(&self) -> LineColumn {
        self.else_branch.as_ref().map_or_else(|| self.brace.span.end(), Located::end)
    }
}

impl<F: HtmlFlavorSpec> Located for HtmlElse<F> {
    fn start(&self) -> LineColumn {
        match self {
            Self::If(r#else, _) => r#else,
            Self::Tree(r#else, _, _) => r#else,
        }
        .span
        .start()
    }

    fn end(&self) -> LineColumn {
        match self {
            Self::If(_, r#if) => r#if.end(),
            Self::Tree(_, brace, _) => brace.span.end(),
        }
    }

    fn loc(&self) -> Location {
        match self {
            Self::If(r#else, r#if) => Location { start: r#else.start(), end: r#if.end() },
            Self::Tree(r#else, brace, _) => {
                Location { start: r#else.span.start(), end: brace.span.end() }
            }
        }
    }
}
