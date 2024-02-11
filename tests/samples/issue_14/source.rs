use
yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    html! {
        <>
            <SomeComponent>
                { "Very long text that will stop these three lines from being merged" }
                // { some_commented_out_content }
            </SomeComponent>
            <@{"div"}>
                { "Very long text that will stop these three lines from being merged" }
                // { some_commented_out_content }
            </@>
            <>
                { "Very long text that will stop these three lines from being merged" }
                // { some_commented_out_content }
            </>
        </>
    }
}
