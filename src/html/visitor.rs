use crate::{
    css,
    html::{
        base::{self, BaseHtmlFlavor},
        ext::{self, ExtHtmlFlavor},
        Html,
    },
};

pub trait Visitor: Sized {
    fn visit_html(&mut self, html: &mut Html) {
        visit_html(self, html)
    }

    fn visit_base_html(&mut self, html: &mut base::Html) {
        visit_base_html(self, html)
    }

    fn visit_base_html_tree(&mut self, tree: &mut base::HtmlTree) {
        visit_base_html_tree(self, tree)
    }

    fn visit_base_html_element(&mut self, element: &mut base::HtmlElement<BaseHtmlFlavor>) {
        visit_base_html_element(self, element)
    }

    fn visit_base_html_fragment(&mut self, fragment: &mut base::HtmlFragment<BaseHtmlFlavor>) {
        visit_base_html_fragment(self, fragment)
    }

    fn visit_base_html_dynamic_element(
        &mut self,
        dynamic: &mut base::HtmlDynamicElement<BaseHtmlFlavor>,
    ) {
        visit_base_html_dynamic_element(self, dynamic)
    }

    fn visit_base_html_literal_element(
        &mut self,
        literal: &mut base::HtmlLiteralElement<BaseHtmlFlavor>,
    ) {
        visit_base_html_literal_element(self, literal)
    }

    fn visit_html_block(&mut self, block: &mut base::HtmlBlock) {
        visit_html_block(self, block)
    }

    fn visit_html_block_content(&mut self, _content: &mut base::HtmlBlockContent) {}

    fn visit_base_html_if(&mut self, if_stmt: &mut base::HtmlIf<BaseHtmlFlavor>) {
        visit_base_html_if(self, if_stmt)
    }

    fn visit_base_html_else(&mut self, else_branch: &mut base::HtmlElse<BaseHtmlFlavor>) {
        visit_base_html_else(self, else_branch)
    }

    fn visit_ext_html(&mut self, html: &mut ext::Html) {
        visit_ext_html(self, html)
    }

    fn visit_ext_html_tree(&mut self, tree: &mut ext::HtmlTree) {
        visit_ext_html_tree(self, tree)
    }

    fn visit_html_for(&mut self, for_stmt: &mut ext::HtmlFor) {
        visit_html_for(self, for_stmt)
    }

    fn visit_html_match(&mut self, match_stmt: &mut ext::HtmlMatch) {
        visit_html_match(self, match_stmt)
    }

    fn visit_html_match_arm(&mut self, arm: &mut ext::HtmlMatchArm) {
        visit_html_match_arm(self, arm)
    }

    fn visit_html_let(&mut self, _let_stmt: &mut ext::HtmlLet) {}

    fn visit_ext_html_if(&mut self, if_stmt: &mut base::HtmlIf<ExtHtmlFlavor>) {
        visit_ext_html_if(self, if_stmt)
    }

    fn visit_ext_html_else(&mut self, else_branch: &mut base::HtmlElse<ExtHtmlFlavor>) {
        visit_ext_html_else(self, else_branch)
    }

    fn visit_ext_html_element(&mut self, element: &mut base::HtmlElement<ExtHtmlFlavor>) {
        visit_ext_html_element(self, element)
    }

    fn visit_ext_html_fragment(&mut self, fragment: &mut base::HtmlFragment<ExtHtmlFlavor>) {
        visit_ext_html_fragment(self, fragment)
    }

    fn visit_ext_html_dynamic(&mut self, dynamic: &mut base::HtmlDynamicElement<ExtHtmlFlavor>) {
        visit_ext_html_dynamic(self, dynamic)
    }

    fn visit_ext_html_literal_element(
        &mut self,
        literal: &mut base::HtmlLiteralElement<ExtHtmlFlavor>,
    ) {
        visit_ext_html_literal_element(self, literal)
    }

    fn visit_html_prop(&mut self, prop: &mut base::HtmlProp) {
        visit_html_prop(self, prop)
    }

    fn visit_style_string(&mut self, _style: &mut css::StyleString) {}
}

pub fn visit_html<V: Visitor>(visitor: &mut V, html: &mut Html) {
    match html {
        Html::Base(base) => visitor.visit_base_html(base),
        Html::Ext(ext) => visitor.visit_ext_html(ext),
    }
}

pub fn visit_base_html<V: Visitor>(visitor: &mut V, html: &mut base::Html) {
    match html {
        base::Html::Tree(tree) => visitor.visit_base_html_tree(tree),
        base::Html::Value(value) => visitor.visit_html_block_content(value),
    }
}

pub fn visit_base_html_tree<V: Visitor>(visitor: &mut V, tree: &mut base::HtmlTree) {
    match tree {
        base::HtmlTree::Element(element) => visitor.visit_base_html_element(element),
        base::HtmlTree::Block(block) => visitor.visit_html_block(block),
        base::HtmlTree::If(if_stmt) => visitor.visit_base_html_if(if_stmt),
    }
}

pub fn visit_base_html_element<V: Visitor>(
    visitor: &mut V,
    element: &mut base::HtmlElement<BaseHtmlFlavor>,
) {
    match element {
        base::HtmlElement::Fragment(fragment) => visitor.visit_base_html_fragment(fragment),
        base::HtmlElement::Dynamic(dynamic) => visitor.visit_base_html_dynamic_element(dynamic),
        base::HtmlElement::Literal(literal) => visitor.visit_base_html_literal_element(literal),
    }
}

pub fn visit_base_html_fragment<V: Visitor>(
    visitor: &mut V,
    fragment: &mut base::HtmlFragment<BaseHtmlFlavor>,
) {
    if let Some(key) = &mut fragment.key {
        visitor.visit_html_prop(key);
    }
    for child in &mut fragment.children {
        visitor.visit_base_html_tree(child);
    }
}

pub fn visit_base_html_dynamic_element<V: Visitor>(
    visitor: &mut V,
    dynamic: &mut base::HtmlDynamicElement<BaseHtmlFlavor>,
) {
    for prop in &mut dynamic.props {
        visitor.visit_html_prop(prop);
    }
    for child in &mut dynamic.children {
        visitor.visit_base_html_tree(child);
    }
}

pub fn visit_base_html_literal_element<V: Visitor>(
    visitor: &mut V,
    literal: &mut base::HtmlLiteralElement<BaseHtmlFlavor>,
) {
    for prop in &mut literal.props {
        visitor.visit_html_prop(prop);
    }
    for child in &mut literal.children {
        visitor.visit_base_html_tree(child);
    }
}

pub fn visit_html_block<V: Visitor>(visitor: &mut V, block: &mut base::HtmlBlock) {
    if let Some(content) = &mut block.content {
        visitor.visit_html_block_content(content);
    }
}

pub fn visit_base_html_if<V: Visitor>(visitor: &mut V, if_stmt: &mut base::HtmlIf<BaseHtmlFlavor>) {
    for child in &mut if_stmt.then_branch {
        visitor.visit_base_html_tree(child);
    }
    if let Some(else_branch) = &mut if_stmt.else_branch {
        visitor.visit_base_html_else(else_branch);
    }
}

pub fn visit_base_html_else<V: Visitor>(
    visitor: &mut V,
    else_branch: &mut base::HtmlElse<BaseHtmlFlavor>,
) {
    match else_branch {
        base::HtmlElse::If(_, if_stmt) => visitor.visit_base_html_if(if_stmt),
        base::HtmlElse::Tree(_, _, children) => {
            for child in children {
                visitor.visit_base_html_tree(child);
            }
        }
    }
}

pub fn visit_ext_html<V: Visitor>(visitor: &mut V, html: &mut ext::Html) {
    match html {
        ext::Html::Tree(trees) => {
            for tree in trees {
                visitor.visit_ext_html_tree(tree);
            }
        }
        ext::Html::Value(value) => visitor.visit_html_block_content(value),
    }
}

pub fn visit_ext_html_tree<V: Visitor>(visitor: &mut V, tree: &mut ext::HtmlTree) {
    match tree {
        ext::HtmlTree::Element(element) => visitor.visit_ext_html_element(element),
        ext::HtmlTree::Block(block) => visitor.visit_html_block(block),
        ext::HtmlTree::If(if_stmt) => visitor.visit_ext_html_if(if_stmt),
        ext::HtmlTree::For(for_stmt) => visitor.visit_html_for(for_stmt),
        ext::HtmlTree::Match(match_stmt) => visitor.visit_html_match(match_stmt),
        ext::HtmlTree::Let(let_stmt) => visitor.visit_html_let(let_stmt),
    }
}

pub fn visit_html_for<V: Visitor>(visitor: &mut V, for_stmt: &mut ext::HtmlFor) {
    for child in &mut for_stmt.body {
        visitor.visit_ext_html_tree(child);
    }
}

pub fn visit_html_match<V: Visitor>(visitor: &mut V, match_stmt: &mut ext::HtmlMatch) {
    for arm in &mut match_stmt.arms {
        visitor.visit_html_match_arm(arm);
    }
}

pub fn visit_html_match_arm<V: Visitor>(visitor: &mut V, arm: &mut ext::HtmlMatchArm) {
    visitor.visit_ext_html_tree(&mut arm.body);
}

pub fn visit_ext_html_if<V: Visitor>(visitor: &mut V, if_stmt: &mut base::HtmlIf<ExtHtmlFlavor>) {
    for child in &mut if_stmt.then_branch {
        visitor.visit_ext_html_tree(child);
    }
    if let Some(else_branch) = &mut if_stmt.else_branch {
        visitor.visit_ext_html_else(else_branch);
    }
}

pub fn visit_ext_html_else<V: Visitor>(
    visitor: &mut V,
    else_branch: &mut base::HtmlElse<ExtHtmlFlavor>,
) {
    match else_branch {
        base::HtmlElse::If(_, if_stmt) => visitor.visit_ext_html_if(if_stmt),
        base::HtmlElse::Tree(_, _, children) => {
            for child in children {
                visitor.visit_ext_html_tree(child);
            }
        }
    }
}

pub fn visit_ext_html_element<V: Visitor>(
    visitor: &mut V,
    element: &mut base::HtmlElement<ExtHtmlFlavor>,
) {
    match element {
        base::HtmlElement::Fragment(fragment) => visitor.visit_ext_html_fragment(fragment),
        base::HtmlElement::Dynamic(dynamic) => visitor.visit_ext_html_dynamic(dynamic),
        base::HtmlElement::Literal(literal) => visitor.visit_ext_html_literal_element(literal),
    }
}

pub fn visit_ext_html_fragment<V: Visitor>(
    visitor: &mut V,
    fragment: &mut base::HtmlFragment<ExtHtmlFlavor>,
) {
    if let Some(key) = &mut fragment.key {
        visitor.visit_html_prop(key);
    }
    for child in &mut fragment.children {
        visitor.visit_ext_html_tree(child);
    }
}

pub fn visit_ext_html_dynamic<V: Visitor>(
    visitor: &mut V,
    dynamic: &mut base::HtmlDynamicElement<ExtHtmlFlavor>,
) {
    for prop in &mut dynamic.props {
        visitor.visit_html_prop(prop);
    }
    for child in &mut dynamic.children {
        visitor.visit_ext_html_tree(child);
    }
}

pub fn visit_ext_html_literal_element<V: Visitor>(
    visitor: &mut V,
    literal: &mut base::HtmlLiteralElement<ExtHtmlFlavor>,
) {
    for prop in &mut literal.props {
        visitor.visit_html_prop(prop);
    }
    for child in &mut literal.children {
        visitor.visit_ext_html_tree(child);
    }
}

pub fn visit_html_prop<V: Visitor>(visitor: &mut V, prop: &mut base::HtmlProp) {
    match &mut prop.kind {
        base::HtmlPropKind::Shortcut(_, _) => {}
        base::HtmlPropKind::Literal(_, _, _) => {}
        base::HtmlPropKind::Block(_, _, _) => {}
        base::HtmlPropKind::Style(_, _, style) => visitor.visit_style_string(style),
    }
}
