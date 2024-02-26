pub mod base;
pub mod ext;

use crate::{formatter::{FmtBlock, FmtCtx, Format, Located, Spacing}, utils::Result};
use proc_macro2::TokenStream;
use serde::Deserialize;
use syn::{parse::Parse, parse2};

use self::{base::BaseHtmlFlavor, ext::ExtHtmlFlavor};

pub trait HtmlFlavorSpec {
    type Root: Parse + Format + Located;
    type Tree: Parse + Format + Located;

    fn element_children_spacing(ctx: &FmtCtx, children: &[Self::Tree]) -> Option<Spacing>;
}

#[derive(Clone, Copy, Deserialize)]
pub enum HtmlFlavor {
    Base,
    Ext,
}

pub enum Html {
    Base(<BaseHtmlFlavor as HtmlFlavorSpec>::Root),
    Ext(<ExtHtmlFlavor as HtmlFlavorSpec>::Root),
}

impl Format for Html {
    fn format<'src>(&self, block: &mut FmtBlock<'_, 'src>, ctx: &mut FmtCtx<'_, 'src>) -> Result {
        match self {
            Html::Base(base) => base.format(block, ctx),
            Html::Ext(ext) => ext.format(block, ctx),
        }
    }
}

impl HtmlFlavor {
    pub fn parse_root(self, input: TokenStream) -> syn::Result<Html> {
        match self {
            Self::Base => parse2(input).map(Html::Base),
            Self::Ext => parse2(input).map(Html::Ext),
        }
    }
}
