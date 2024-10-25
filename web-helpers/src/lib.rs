use web_prelude::{document, HtmlAnyElement, HtmlDivElement};

fn div() -> HtmlDivElement {
    let div = document().create_element("div");
    unsafe { std::mem::transmute::<HtmlAnyElement, HtmlDivElement>(div) }
}
