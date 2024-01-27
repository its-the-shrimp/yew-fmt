use yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    html! { <><Smth ..props /><Smth ..props>{ "Something else" }</Smth></> }
}
