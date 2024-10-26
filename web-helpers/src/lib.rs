use web_prelude::{document, Element, HtmlAnyElement, HtmlDivElement, HtmlElement, Node};


// TODO this could probably all be inline away and then optimised down to the equivalent imperative js (though may require some iifes), though is somewhat complicated because whilst:
// ```js
// document.createElement("div").appendChild(document.createElement("span"))
// ```
// is valid JS, it returns the span, not the div, and the below is not valid since `.add()` returns None/undefined:
// ```js
// document.createElement("div").classList.add("wrapper").appendChild(document.createElement("span"))
// ```
// So the inlined and more imperative code may actually be less space effecient than keeping this abstraction - only way to know is to test the compressed size.


struct Div(HtmlDivElement);

impl Div {
    fn new() -> Div {
        let any_div = document().create_element("div");
        let typed_div = unsafe { std::mem::transmute::<HtmlAnyElement, HtmlDivElement>(any_div) };
        Div(typed_div)
    }
    fn class(self, class_name: &str) -> Div {
        self.0.class_list().add(class_name);
        self
    }
    fn child(self, child: impl HtmlElement) -> Div {
        self.0.append_child(child);
        self
    }
}
