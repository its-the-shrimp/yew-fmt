use yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    html! {
        <MyComponent>
            { very_long_arg_name_example1 }
            { very_long_arg_name_example2 }
            // { very_long_arg_name_example3 }
            // { very_long_arg_name_example4 }
        </MyComponent>
    }
}
