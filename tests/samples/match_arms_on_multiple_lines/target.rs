// config: yew.html_flavor="Ext"

use yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    html! {
        match x {
            A::B => {},
            A::C => {},
        }
    }
}
