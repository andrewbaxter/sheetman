use {
    gloo::{
        events::EventListener,
        utils::document,
    },
    rooting::{
        el,
        set_root,
    },
    std::{
        cell::RefCell,
        rc::Rc,
    },
    wasm::{
        create_editor,
        dom::{
            as_blob,
            ICON_CLIPBOARD,
            ICON_OPEN,
            ICON_SAVE,
        },
        EditorAction,
        EditorOptions,
        Hotkey,
    },
    wasm_bindgen::JsCast,
    web_sys::{
        FileReader,
        HtmlElement,
        HtmlInputElement,
        Url,
    },
};

fn main() {
    console_error_panic_hook::set_once();
    let hidden = el("div").classes(&["hidden"]);
    let filename = Rc::new(RefCell::new("sheet.jsv".to_string()));
    set_root(vec![create_editor(EditorOptions {
        sheet_actions: vec![
            //. .
            EditorAction {
                text: "Open".to_string(),
                icon: ICON_OPEN.to_string(),
                hotkeys: vec![Hotkey {
                    key: "o".to_string(),
                    ctrl: true,
                    alt: false,
                }],
                cb: Rc::new(RefCell::new(Box::new({
                    let hidden = hidden.clone();
                    let filename = filename.clone();
                    move |editor| {
                        let file_dialog_input = el("input").attr("type", "file").attr("accept", ".jsv, .json");
                        file_dialog_input.ref_on("change", {
                            let filename = filename.clone();
                            let file_dialog_input = file_dialog_input.clone();
                            let editor = editor.clone();
                            move |ev| {
                                let e = ev.target().unwrap().dyn_into::<HtmlInputElement>().unwrap();
                                let files = e.files().unwrap();
                                if files.length() == 0 {
                                    return;
                                }
                                let f = files.item(0).unwrap();
                                let reader = FileReader::new().unwrap();
                                let cb_handler = EventListener::new(&reader, "load", {
                                    let reader = reader.clone();
                                    let editor = editor.clone();
                                    let filename = filename.clone();
                                    let name = f.name();
                                    move |_| {
                                        editor
                                            .set_data(
                                                serde_json::from_str(
                                                    &reader.result().unwrap().as_string().unwrap(),
                                                ).unwrap(),
                                            )
                                            .unwrap();
                                        *filename.borrow_mut() = name.clone();
                                    }
                                });
                                reader.read_as_text(&f).unwrap();

                                // Keep alive in file input which will be cleaned up next time someone presses the
                                // button
                                file_dialog_input.ref_own(move |_| cb_handler);
                            }
                        });
                        file_dialog_input.raw().dyn_into::<HtmlElement>().unwrap().click();
                        hidden.ref_clear();
                        hidden.ref_push(file_dialog_input);
                    }
                }))),
            },
            EditorAction {
                text: "Save".to_string(),
                icon: ICON_SAVE.to_string(),
                hotkeys: vec![Hotkey {
                    key: "s".to_string(),
                    ctrl: true,
                    alt: false,
                }],
                cb: Rc::new(RefCell::new(Box::new({
                    let filename = filename.clone();
                    move |editor| {
                        let document = document();
                        let body = document.body().unwrap();
                        let a = document.create_element("a").unwrap().dyn_into::<HtmlElement>().unwrap();
                        let url = Url::create_object_url_with_blob(&as_blob(editor.get_data())).unwrap();
                        a.set_attribute("href", &url).unwrap();
                        a.set_attribute("download", &filename.borrow()).unwrap();
                        body.append_child(&a).unwrap();
                        a.click();
                        body.remove_child(&a).unwrap();
                        Url::revoke_object_url(&url).unwrap();
                    }
                }))),
            },
            EditorAction {
                text: "Copy".to_string(),
                icon: ICON_CLIPBOARD.to_string(),
                hotkeys: vec![],
                cb: Rc::new(RefCell::new(Box::new(|editor| {
                    wasm::dom::copy(editor.get_data());
                }))),
            }
        ],
        initial_data: vec![
            serde_json::from_value(
                serde_json::Value::Object(
                    [("".to_string(), serde_json::Value::String("".to_string()))].into_iter().collect(),
                ),
            ).unwrap()
        ],
    }).unwrap().root(), hidden]);
}
