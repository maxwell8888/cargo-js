use ravascript::web::*;

fn expanding_textarea() -> HTMLDivElement {
    let div = Document::create_element_div();
    let div_class = "grid after::content[content: attr(data-replicated-value) ' '] after::whitespace-pre-wrap after::invisible after::border-2 after::border-black after::p-2 after::row-start-1 after::row-end-2 after::col-start-1 after::col-end-2";
    div.set_attribute("class", div_class);
    let textarea = Document::create_element_textarea();
    let textarea_class = "resize-none overflow-hidden border-2 border-black p-2 row-start-1 row-end-2 col-start-1 col-end-2";
    textarea.set_attribute("class", textarea_class);
    textarea.set_attribute(
        "onInput",
        "this.parentNode.dataset.replicatedValue = this.value",
    );
    div.append_child(textarea);
    div
}

struct Company {
    name: String,
    location: Option<String>,
    link: Option<String>,
}
struct Position {
    company_name: Company,
    job_title: String,
    start_date: Date,
    end_date: Option<Date>,
}
struct Schema {
    positions: Vec<Position>,
}

struct Post {
    id: usize,
    title: String,
    body: String,
    userId: usize,
}

#[tokio::main]
async fn main() {
    let body = Document::query_selector_body();
    let div = Document::create_element_div();
    let text = Document::create_text_node("hello");
    div.append_child(text);
    body.append_child(div);

    let positions = vec![
        Position {
            company_name: Company {
                name: "Cyberdyne Systems".to_string(),
                location: None,
                link: None,
            },
            job_title: "AI researcher".to_string(),
            start_date: Date::from_iso_string("2024-05-12T23:50:21.817Z"),
            end_date: None,
        },
        Position {
            company_name: Company {
                name: "Open AI".to_string(),
                location: None,
                link: None,
            },
            job_title: "AI researcher".to_string(),
            start_date: Date::from_iso_string("2020-05-12T23:50:21.817Z"),
            end_date: Some(Date::from_iso_string("2024-05-12T23:50:21.817Z")),
        },
    ];
    let schema = Schema { positions };
    let pos_divs = schema
        .positions
        .iter()
        .map(|position| {
            let div = Document::create_element_div();
            let name = Document::create_text_node(&position.company_name.name);
            div.append_child(name);
            div
        })
        .collect::<Vec<_>>();
    for pos_div in pos_divs {
        body.append_child(pos_div);
    }

    let response = fetch("https://jsonplaceholder.typicode.com/posts").await;
    let posts = response.json::<Vec<Post>>().await;
    Console::log(&posts);
    let posts_div = Document::create_element_div();
    posts_div.set_attribute("style", "display: flex; flex-direction: column; gap: 10px");
    for post in posts {
        let post_div = Document::create_element_div();
        let title_div = Document::create_element_div();
        title_div.append_child(Document::create_text_node(post.title));
        let body_div = Document::create_element_div();
        body_div.append_child(Document::create_text_node(post.body));
        post_div.append_child(title_div);
        post_div.append_child(body_div);
        posts_div.append_child(post_div)
    }
    body.append_child(posts_div);

    // Fullscreen example
    fn toggle_full_screen(event: Event) {
        // TODO support is_none
        // if Document::FULLSCREEN_ELEMENT.is_none() {
        if !Document::FULLSCREEN_ELEMENT {
            Document::DOCUMENT_ELEMENT.request_fullscreen();
        } else if Document::EXIT_FULLSCREEN {
            Document::exit_fullscreen();
        }
    }
    let fullscreen_button = Document::create_element("button");
    fullscreen_button.add_event_listener("click", toggle_full_screen);
    fullscreen_button.append_child(Document::create_text_node("full screen"));
    body.append_child(fullscreen_button);

    // Clipboard text example
    // TODO We want to capture `input` in the event handler so want to use a closure rather than a fn, however Rust doesn't currently support async closures so we are not able to await `Navigator::CLIPBOARD.write_text()` which means we can't take any subsequent actions which rely on the text being written to the clipboard having finished. We continue to use the async `add_event_listener` so that we can return the clipboard future from the event handler and avoid warnings about unawaited futures.
    let input = Document::create_element2::<HTMLInputElement>("input");
    let button = Document::create_element("button");
    button.append_child(Document::create_text_node("Copy to clipboard"));
    let get_text = |_event: Event| async { NAVIGATOR.clipboard.write_text(input.value).await };
    button.add_event_listener_async("click", get_text);
    body.append_child(input);
    body.append_child(button);

    let button = Document::create_element("button");
    button.append_child(Document::create_text_node("Paste from clipboard"));
    let get_text = |_event: Event| async {
        let text = NAVIGATOR.clipboard.read_text().await;
        body.append_child(Document::create_text_node(text));
    };
    button.add_event_listener_async("click", get_text);
    body.append_child(button);

    // Clipboard arbitrary data example
}
