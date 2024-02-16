// config: yew.ext=true
use yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    html! {
        for i in 0 .. 10 {
            <code>{ i }</code>
            <br />
        }
    }
}
