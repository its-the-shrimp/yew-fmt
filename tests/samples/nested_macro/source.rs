use yew::prelude::*;

#[function_component(Application)]
fn app() -> Html { 
let names = vec!["Sam","Bob","Ray"];

html! {
<div id="introductions">
{
names.into_iter().map(|name| {
html!{<div key={name}>{ format!("Hello, I'am {}!",name) }</div>}
}).collect::<Html>()
}
</div>
};
}