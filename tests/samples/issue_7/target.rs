use yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    html! {};
    html!();
    html!();
    html!(
        if true {
            { "true" }
        } else {
            { "false" }
        }
    )
}
