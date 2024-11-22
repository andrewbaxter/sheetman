use {
    gloo_utils::format::JsValueSerdeExt,
    rooting::set_root,
    std::{
        cell::RefCell,
        rc::Rc,
    },
    wasm::{
        create_editor,
        dom::{
            ICON_SAVE,
            ICON_SAVEAS,
        },
        EditorAction,
        EditorOptions,
        Hotkey,
    },
    wasm_bindgen::{
        prelude::wasm_bindgen,
        JsValue,
    },
    wasm_bindgen_futures::{
        js_sys::Object,
        spawn_local,
    },
};

#[wasm_bindgen]
extern "C" {
    // invoke without arguments
    #[wasm_bindgen(js_namespace = ["window", "__TAURI__", "core"], js_name = invoke)]
    async fn invoke_without_args(cmd: &str) -> JsValue;

    // invoke with arguments (default)
    #[wasm_bindgen(js_namespace = ["window", "__TAURI__", "core"])]
    async fn invoke(cmd: &str, args: JsValue) -> JsValue;
    // They need to have different names!
}

fn main() {
    console_error_panic_hook::set_once();
    spawn_local(async {
        set_root(vec![create_editor(EditorOptions {
            sheet_actions: vec![
                //. .
                EditorAction {
                    text: "Save".to_string(),
                    icon: ICON_SAVE.to_string(),
                    hotkeys: vec![Hotkey {
                        key: "s".to_string(),
                        ctrl: true,
                        alt: false,
                    }],
                    cb: Rc::new(RefCell::new(Box::new(|editor| {
                        spawn_local(async move {
                            invoke("command_save", JsValue::from(Object::from_entries(&JsValue::from(vec![
                                //. .
                                JsValue::from(vec![JsValue::from("data"), JsValue::from_serde(&editor.get_data()).unwrap()])
                            ])).unwrap())).await;
                        });
                    }))),
                },
                EditorAction {
                    text: "Save As".to_string(),
                    icon: ICON_SAVEAS.to_string(),
                    hotkeys: vec![],
                    cb: Rc::new(RefCell::new(Box::new(|editor| {
                        spawn_local(async move {
                            invoke("command_save_as", JsValue::from(Object::from_entries(&JsValue::from(vec![
                                //. .
                                JsValue::from(vec![JsValue::from("data"), JsValue::from_serde(&editor.get_data()).unwrap()])
                            ])).unwrap())).await;
                        });
                    }))),
                }
            ],
            initial_data: invoke_without_args("command_take_initial_data").await.into_serde().unwrap(),
        }).unwrap().root()]);
    });
}
