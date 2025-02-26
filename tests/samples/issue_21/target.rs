// config: hard_tabs=true

use yew::prelude::*;

fn f() -> Html {
	html! {
		<div>
			<h1>{ "One" }</h1>
			<h2>{ "Two" }</h2>
			// Comment
		</div>
	}
}

fn view(&self, ctx: &Context<Self>) -> Html {
	html! {
		<div>
			{ "I am a card" }
			<h1>{ "I am the title" }</h1>
			// TODO: add more contents
		</div>
	}
}
