// config: yew.ext=false
// the following is an html! invocation that's valid in base Yew syntax, but invalid in
// yew-html-ext
use
yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    html! {
        match x {
            42 => <Foo as Bar>::idk(),
            _ => html!(),
        }
    }
}
