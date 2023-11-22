use yew::prelude::*;

#[function_component]
pub fn Main() -> Html {
    // smth
    html! { <div id="wrapper"> <p id="title">{ "Upload Your Files To The Cloud" }</p> <label for="file-upload"> <div id="drop-container" ondrop={ctx.link().callback(|event: DragEvent| { event.prevent_default(); let files = event.data_transfer().unwrap().files(); Self::upload_files(files) })} ondragover={Callback::from(|event: DragEvent| { event.prevent_default(); })} ondragenter={Callback::from(|event: DragEvent| { event.prevent_default(); })} > <i class="fa fa-cloud-upload"></i> <p>{"Drop your images here or click to select"}</p> </div> </label> <input id="file-upload" type="file" accept="image/*,video/*" multiple={true} onchange={ctx.link().callback(move |e: Event| { let input: HtmlInputElement = e.target_unchecked_into(); Self::upload_files(input.files()) })} /> <div id="preview-area"> { for self.files.iter().map(Self::view_file) } </div> </div> }
}
