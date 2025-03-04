use
yew::prelude::*;

#[function_component]
pub fn MyComponent() -> Html {
    html! {
        <Select<<Type as Weight>::Unit>
            class={classes!("color-red-500")}
            {on_change}
        />
    }
}
