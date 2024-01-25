use
yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    let loading=true;

    html! { if true { { "true" } } else { { "false" } } }
}
