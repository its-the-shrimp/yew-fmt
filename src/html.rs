use proc_macro2::{Delimiter, TokenTree, TokenStream};
use quote::ToTokens;
use syn::{
    braced,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Expr, Ident, Token, Type, token::Brace, buffer::Cursor, Lit, ext::IdentExt,
};

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
