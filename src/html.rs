use std::ops::Deref;

use proc_macro2::{Delimiter, TokenTree, TokenStream, LineColumn};
use quote::ToTokens;
use syn::{
    braced,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Expr, Ident, Token, Type, token::Brace, buffer::Cursor, Lit, ext::IdentExt, spanned::Spanned,
};
use anyhow::Result;
use crate::formatter::{Format, FmtBlock, FormatCtx, Spacing, Located, Location};

/// Overrides `Ident`'s default `Parse` behaviour by accepting Rust keywords
pub struct AnyIdent(Ident);

impl Parse for AnyIdent {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ident::parse_any(input).map(Self)
    }
}

impl ToTokens for AnyIdent {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens)
    }
}

impl Deref for AnyIdent {
    type Target = Ident;
    fn deref(&self) -> &Self::Target { &self.0 }
}

pub enum Html {
    Tree(HtmlTree),
    Value(Box<HtmlBlockContent>),
}

pub enum HtmlTree {
    Element(Box<HtmlElement>),
    Block(Box<HtmlBlock>),
    If(Box<HtmlIf>),
}

pub struct HtmlBlock {
    pub brace: Brace,
    pub content: HtmlBlockContent
}

pub enum HtmlBlockContent {
    Expr(Expr),
    Iterable(Token![for], Expr),
}

pub enum HtmlElement {
    Fragment(HtmlFragment),
    Dynamic(HtmlDynamicElement),
    Literal(HtmlLiteralElement),
}

pub struct HtmlFragment {
    pub lt_token: Token![<],
    pub key: Option<HtmlProp>,
    pub gt_token: Token![>],
    pub children: Vec<HtmlTree>,
    pub closing_lt_token: Token![<],
    pub div_token: Token![/],
    pub closing_gt_token: Token![>]
}

pub struct HtmlDynamicElement {
    pub lt_token: Token![<],
    pub at_token: Token![@],
    pub name: BracedExpr,
    pub props: Vec<HtmlProp>,
    pub children: Vec<HtmlTree>,
    pub closing_tag: Option<(Token![>], Token![<], Token![@])>,
    pub div_token: Token![/],
    pub closing_gt_token: Token![>]
}

pub struct HtmlLiteralElement {
    pub lt_token: Token![<],
    pub name: TokenStream,
    pub props: Vec<HtmlProp>,
    pub prop_base: Option<(Token![..], Expr)>,
    pub children: Vec<HtmlTree>,
    pub closing_tag: Option<(Token![>], Token![<], TokenStream)>,
    pub div_token: Token![/],
    pub closing_gt_token: Token![>]
}

pub struct HtmlProp {
    pub access_spec: Option<Token![~]>,
    pub kind: HtmlPropKind
}

pub enum HtmlPropKind {
    Shortcut(Brace, Punctuated<AnyIdent, Token![-]>),
    Literal(Punctuated<AnyIdent, Token![-]>, Token![=], Lit),
    Expr(Punctuated<AnyIdent, Token![-]>, Token![=], BracedExpr),
}

pub struct BracedExpr {
    pub brace: Brace,
    pub expr: Expr,
}

pub struct HtmlIf {
    pub if_token: Token![if],
    pub condition: Expr,
    pub brace: Brace,
    pub then_branch: Vec<HtmlTree>,
    pub else_branch: Option<HtmlElse>,
}

pub enum HtmlElse {
    If(Token![else], Box<HtmlIf>),
    Tree(Token![else], Brace, Vec<HtmlTree>),
}

impl Parse for Html {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(if HtmlTree::parseable(input.cursor()) {
            Self::Tree(input.parse()?)
        } else {
            Self::Value(Box::new(input.parse()?))
        })
    }
}

impl Parse for HtmlTree {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(if HtmlIf::parseable(input.cursor()) {
            Self::If(Box::new(input.parse()?))
        } else if HtmlElement::parseable(input.cursor()) {
            Self::Element(Box::new(input.parse()?))
        } else {
            Self::Block(Box::new(input.parse()?))
        })
    }
}

impl HtmlBlock {
    fn parseable(cursor: Cursor) -> bool {
        cursor.group(Delimiter::Brace).is_some()
    }
}

impl Parse for HtmlBlock {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        let brace = braced!(content in input);
        HtmlBlockContent::parse(&content)
            .map(|content| Self { content, brace })
    }
}

impl Parse for HtmlBlockContent {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(if let Ok(for_token) = input.parse() {
            Self::Iterable(for_token, input.parse()?)
        } else {
            Self::Expr(input.parse()?)
        })
    }
}

impl HtmlElement {
    fn parseable(cursor: Cursor) -> bool {
        cursor.punct().is_some_and(|(p, _)| p.as_char() == '<')
    }
}

impl Parse for HtmlElement {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(if HtmlFragment::parseable(input.cursor()) {
            Self::Fragment(input.parse()?)
        } else if HtmlDynamicElement::parseable(input.cursor()) {
            Self::Dynamic(input.parse()?)
        } else {
            Self::Literal(input.parse()?)
        })
    }
}

impl HtmlFragment {
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

impl Parse for HtmlFragment {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lt_token = input.parse()?;
        let (key, gt_token) = if input.peek(Token![>]) {
            (None, input.parse()?)
        } else {
            (Some(input.parse()?), input.parse()?)
        };
        let children = HtmlTree::parse_children(input)?;
        let closing_lt_token = input.parse()?;
        let div_token = input.parse()?;
        let closing_gt_token = input.parse()?;

        Ok(Self {
            lt_token,
            key,
            gt_token,
            children,
            closing_lt_token,
            div_token,
            closing_gt_token
        })
    }
}

impl Parse for HtmlDynamicElement {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lt_token = input.parse()?;
        let at_token = input.parse()?;
        let name = input.parse()?;

        let mut props = vec![];
        while input.cursor().punct().is_none() {
            props.push(input.parse()?)
        }

        let (children, closing_tag) = if input.peek(Token![>]) {
            let gt_token = input.parse()?;
            (
                HtmlTree::parse_children(input)?,
                Some((gt_token, input.parse()?, input.parse()?))
            )
        } else {
            (vec![], None)
        };

        let div_token = input.parse()?;
        let closing_gt_token = input.parse()?;
        Ok(Self {
            lt_token,
            at_token,
            name,
            props,
            children,
            closing_tag,
            div_token,
            closing_gt_token
        })
    }
}

impl Parse for HtmlLiteralElement {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lt_token = input.parse()?;

        fn get_name(input: ParseStream) -> syn::Result<TokenStream> {
            Ok(if let Ok(ty) = Type::parse(input) {
                ty.into_token_stream()
            } else {
                let name = Punctuated::<AnyIdent, Token![-]>::parse_separated_nonempty(input)?;
                name.into_token_stream()
            })
        }
        let name = get_name(input)?;

        let mut props = vec![];
        while input.cursor().punct().is_none() {
            props.push(input.parse()?)
        }

        let prop_base = if input.peek(Token![..]) {
            Some((input.parse()?, input.parse()?))
        } else {
            None  
        };

        let (children, closing_tag, div_token) = if input.peek(Token![>]) {
            let gt_token = input.parse()?;
            let children = HtmlTree::parse_children(input)?;
            let closing_lt_token = input.parse()?;
            let div_token = input.parse()?;
            let closing_name = get_name(input)?;
            (
                children,
                Some((gt_token, closing_lt_token, closing_name)),
                div_token,
            )
        } else {
            (vec![], None, input.parse()?)
        };

        let closing_gt_token = input.parse()?;

        Ok(Self {
            lt_token,
            name,
            props,
            prop_base,
            children,
            closing_tag,
            div_token,
            closing_gt_token
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
                HtmlPropKind::Expr(name, eq_token, input.parse()?)
            } else {
                HtmlPropKind::Literal(name, eq_token, input.parse()?)
            }
        };
        Ok(Self { access_spec, kind })
    }
}

impl Parse for BracedExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let inner;
        let brace = braced!(inner in input);
        Ok(Self { brace, expr: inner.parse()? })
    }
}

impl Parse for HtmlIf {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let if_token = input.parse()?;
        let condition = Expr::parse_without_eager_brace(input)?;
        let then_block;
        let brace = braced!(then_block in input);
        let then_branch = HtmlTree::parse_children(&then_block)?;
        let else_branch = HtmlElse::parseable(input.cursor())
            .then(|| input.parse()).transpose()?;
        Ok(Self {
            if_token,
            condition,
            brace,
            then_branch,
            else_branch,
        })
    }
}

impl Parse for HtmlElse {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let else_token = <Token![else]>::parse(input)?;
        Ok(if HtmlIf::parseable(input.cursor()) {
            Self::If(else_token, Box::new(input.parse()?))
        } else {
            let inner;
            let brace = braced!(inner in input);
            Self::Tree(else_token, brace, HtmlTree::parse_children(&inner)?)
        })
    }
}

impl HtmlTree {
    fn parseable(cursor: Cursor<'_>) -> bool {
        cursor.eof()
        || HtmlIf::parseable(cursor)
        || HtmlElement::parseable(cursor)
        || HtmlBlock::parseable(cursor)
    }

    fn parse_children(input: ParseStream) -> syn::Result<Vec<Self>> {
        let mut res = vec![];
        while !(input.peek(Token![<]) && input.peek2(Token![/])) {
            res.push(input.parse()?)
        }
        Ok(res)
    }
}

impl HtmlDynamicElement {
    fn parseable(cursor: Cursor) -> bool {
        let Some((_, cursor)) = cursor.punct().filter(|(p, _)| p.as_char() == '<') else {
            return false;
        };
        cursor.punct().is_some_and(|(p, _)| p.as_char() == '@')
    }
}

impl HtmlIf {
    fn parseable(cursor: Cursor) -> bool {
        cursor.ident().is_some_and(|(i, _)| i == "if")
    }
}

impl HtmlElse {
    fn parseable(cursor: Cursor) -> bool {
        cursor.ident().is_some_and(|(i, _)| i == "else")
    }
}

const ELEMENT_CHILDREN_SPACING: Spacing = Spacing { before: false, between: false, after: false };
const   BLOCK_CHILDREN_SPACING: Spacing = Spacing { before: true,  between: false, after: true  };

const fn props_spacing(self_closing: bool) -> Spacing {
    Spacing { before: true, between: true, after: self_closing }
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
        block.add_source(ctx, self.lt_token.loc())?;
        if let Some(key) = &self.key {
            key.format(block, ctx)?;
        }
        block.add_source(ctx, self.gt_token.loc())?;

        block.add_block(ELEMENT_CHILDREN_SPACING, |block| anyhow::Ok({
            for child in &self.children {
                child.format(block, ctx)?;
                block.add_sep(ctx, child.end())?;
            }
        }))?;

        block.add_source(ctx, self.closing_lt_token.loc())?;
        block.add_source(ctx, self.div_token.loc())?;
        block.add_source(ctx, self.closing_gt_token.loc())
    }
}

impl<'src> Format<'src> for HtmlDynamicElement {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        block.add_source(ctx, self.lt_token.loc())?;
        block.add_source(ctx, self.at_token.loc())?;
        self.name.format(block, ctx)?;
        let self_closing = self.closing_tag.is_none() || self.children.is_empty();

        block.add_block(props_spacing(self_closing), |block| anyhow::Ok({
            for prop in &self.props {
                prop.format(block, ctx)?;
                block.add_sep(ctx, prop.end())?;
            }
        }))?;

        let closing_tag = self.closing_tag.as_ref().filter(|_| !self.children.is_empty());
        if let Some((gt, closing_lt, closing_at)) = closing_tag {
            block.add_source(ctx, gt.loc())?;
            block.add_block(ELEMENT_CHILDREN_SPACING, |block| anyhow::Ok({
                for child in &self.children {
                    child.format(block, ctx)?;
                    block.add_sep(ctx, child.end())?;
                }
            }))?;
            block.add_source(ctx, closing_lt.loc())?;
            block.add_source(ctx, self.div_token.loc())?;
            block.add_source(ctx, closing_at.loc())?;
            block.add_source(ctx, self.closing_gt_token.loc())
        } else {
            block.add_source(ctx, self.div_token.loc())?;
            block.add_source(ctx, self.closing_gt_token.loc())
        }
    }
}

impl<'src> Format<'src> for HtmlLiteralElement {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        block.add_source(ctx, self.lt_token.loc())?;
        block.add_source_iter(ctx, self.name.clone())?;
        let self_closing = self.closing_tag.is_none() || self.children.is_empty();

        block.add_block(props_spacing(self_closing), |block| anyhow::Ok({
            for prop in &self.props {
                prop.format(block, ctx)?;
                block.add_sep(ctx, prop.end())?;
            }
        }))?;

        let closing_tag = self.closing_tag.as_ref().filter(|_| !self.children.is_empty());
        if let Some((gt, closing_lt, closing_name)) = closing_tag {
            block.add_source(ctx, gt.loc())?;
            block.add_block(ELEMENT_CHILDREN_SPACING, |block| anyhow::Ok({
                for child in &self.children {
                    child.format(block, ctx)?;
                    block.add_sep(ctx, child.end())?;
                }
            }))?;
            block.add_source(ctx, closing_lt.loc())?;
            block.add_source(ctx, self.div_token.loc())?;
            block.add_source_iter(ctx, closing_name.clone())?;
            block.add_source(ctx, self.closing_gt_token.loc())
        } else {
            block.add_source(ctx, self.div_token.loc())?;
            block.add_source(ctx, self.closing_gt_token.loc())
        }
    }
}

impl<'src> Format<'src> for HtmlProp {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        if let Some(tilde) = &self.access_spec {
            block.add_source(ctx, tilde.loc())?;
        }
        match &self.kind {
            HtmlPropKind::Shortcut(brace, name) => {
                block.add_source(ctx, brace.span.open().loc())?;
                block.add_source_punctuated(ctx, name)?;
                block.add_source(ctx, brace.span.close().loc())
            }
            HtmlPropKind::Literal(name, eq, lit) => {
                block.add_source_punctuated(ctx, name)?;
                block.add_source(ctx, eq.loc())?;
                block.add_source(ctx, lit.loc())
            }
            HtmlPropKind::Expr(name, eq, expr) => {
                if ctx.config.yew.use_prop_init_shorthand
                    && name.len() == 1
                    && matches!(&expr.expr, Expr::Path(p)
                        if p.path.is_ident(unsafe { &**name.first().unwrap_unchecked() }))
                {
                    return expr.format(block, ctx)
                }

                block.add_source_punctuated(ctx, name)?;
                block.add_source(ctx, eq.loc())?;
                if ctx.config.yew.unwrap_literal_prop_values && matches!(expr.expr, Expr::Lit(_)) {
                    block.add_source(ctx, expr.expr.loc())
                } else {
                    expr.format(block, ctx)
                }
            }
        }
    }
}

impl<'src> Format<'src> for BracedExpr {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        block.add_source(ctx, self.brace.span.open().loc())?;
        block.add_source(ctx, self.expr.loc())?;
        block.add_source(ctx, self.brace.span.close().loc())
    }
}

impl<'src> Format<'src> for HtmlBlock {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        block.add_source(ctx, self.brace.span.open().loc())?;
        block.add_space(ctx, self.content.start())?;
        self.content.format(block, ctx)?;
        let closing_brace_loc = self.brace.span.close().loc();
        block.add_space(ctx, closing_brace_loc.start)?;
        block.add_source(ctx, closing_brace_loc)
    }
}

impl<'src> Format<'src> for HtmlBlockContent {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        match self {
            Self::Expr(e) => block.add_source(ctx, e.loc()),
            Self::Iterable(r#for, e) => {
                block.add_source(ctx, r#for.loc())?;
                let expr_loc = e.loc();
                block.add_space(ctx, expr_loc.start())?;
                block.add_source(ctx, expr_loc)
            }
        }
    }
}

impl<'src> Format<'src> for HtmlIf {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        block.add_source(ctx, self.if_token.loc())?;
        let condition_loc = self.condition.loc();
        block.add_space(ctx, condition_loc.start)?;
        block.add_source(ctx, condition_loc)?;

        let opening_brace_loc = self.brace.span.open().loc();
        block.add_space(ctx, opening_brace_loc.start)?;
        block.add_source(ctx, opening_brace_loc)?;
        block.add_block(BLOCK_CHILDREN_SPACING, |block| anyhow::Ok({
            for child in &self.then_branch {
                child.format(block, ctx)?;
                block.add_sep(ctx, child.end())?;
            }
        }))?;
        block.add_source(ctx, self.brace.span.close().loc())?;
        
        Ok(if let Some(else_branch) = &self.else_branch {
            block.add_space(ctx, else_branch.start())?;
            else_branch.format(block, ctx)?;
        })
    }
}

impl<'src> Format<'src> for HtmlElse {
    fn format(&self, block: &mut FmtBlock<'src>, ctx: &mut FormatCtx<'_, 'src>) -> Result<()> {
        match self {
            Self::If(r#else, r#if) => {
                block.add_source(ctx, r#else.loc())?;
                r#if.format(block, ctx)
            }
            Self::Tree(r#else, brace, children) => {
                block.add_source(ctx, r#else.loc())?;
                block.add_source(ctx, brace.span.open().loc())?;
                block.add_block(BLOCK_CHILDREN_SPACING, |block| anyhow::Ok({
                    for child in children {
                        child.format(block, ctx)?;
                        block.add_sep(ctx, child.end())?;
                    }
                }))?;
                block.add_source(ctx, brace.span.close().loc())
            }
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

impl Located for HtmlElement {
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

impl Located for HtmlFragment {
    fn start(&self) -> LineColumn { self.lt_token.span.start() }
    fn end(&self) -> LineColumn { self.closing_gt_token.span.end() }
}

impl Located for HtmlDynamicElement {
    fn start(&self) -> LineColumn { self.lt_token.span.start() }
    fn end(&self) -> LineColumn { self.closing_gt_token.span.end() }
}

impl Located for HtmlLiteralElement {
    fn start(&self) -> LineColumn { self.lt_token.span.start() }
    fn end(&self) -> LineColumn { self.closing_gt_token.span.end() }
}

impl Located for HtmlBlock {
    fn start(&self) -> LineColumn { self.brace.span.open().start() }
    fn end(&self) -> LineColumn { self.brace.span.close().end() }
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
        }.span().end()
    }
    
    fn loc(&self) -> Location {
        match self {
            Self::Expr(expr) =>
                expr.loc(),
            Self::Iterable(r#for, expr) =>
                Location { start: r#for.span.start(), end: expr.end() },
        }
    }
}

impl Located for HtmlIf {
    fn start(&self) -> LineColumn { self.if_token.span.start() }
    fn end(&self) -> LineColumn {
        self.else_branch.as_ref()
            .map_or_else(|| self.brace.span.close().end(), Located::end)
    }
}

impl Located for HtmlElse {
    fn start(&self) -> LineColumn {
        match self {
            Self::If(r#else, _) => r#else,
            Self::Tree(r#else, _, _) => r#else,
        }.span.start()
    }

    fn end(&self) -> LineColumn {
        match self {
            Self::If(_, r#if) => r#if.end(),
            Self::Tree(_, brace, _) => brace.span.close().end(),
        }
    }

    fn loc(&self) -> Location {
        match self {
            Self::If(r#else, r#if) =>
                Location { start: r#else.span.start(), end: r#if.end() },
            Self::Tree(r#else, brace, _) =>
                Location { start: r#else.span.start(), end: brace.span.close().end() },
        }
    }
}

impl Located for HtmlProp {
    fn start(&self) -> LineColumn {
        if let Some(tilde) = &self.access_spec {
            return tilde.span.start()
        }

        match &self.kind {
            HtmlPropKind::Shortcut(brace, _) => brace.span.open().start(),
            HtmlPropKind::Literal(name, ..) | HtmlPropKind::Expr(name, ..) => unsafe {
                // Safety: the name of the prop is guaranteed to be non-empty by
                // `Punctuated::parse_terminated(_nonempty)`
                name.first().unwrap_unchecked().span().start()
            }
        }
    }

    fn end(&self) -> LineColumn {
        match &self.kind {
            HtmlPropKind::Shortcut(brace, _) => brace.span.close().end(),
            HtmlPropKind::Literal(_, _, lit) => lit.span().end(),
            HtmlPropKind::Expr(_, _, expr) => expr.brace.span.close().end(),
        }
    }
}
