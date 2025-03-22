use yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    html! {
        <ul
            style="
                display: flex;
                flex-wrap: wrap;
                gap: 1.5rem;
                padding: 0;
                margin: 0;
                list-style: none;
                border: 1px solid #ccc
            "
        >
            <strong>{ "Hello, World" }</strong>
        </ul>
    }
}
