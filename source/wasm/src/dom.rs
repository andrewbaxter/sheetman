use {
    gloo::utils::window,
    js_sys::Object,
    wasm_bindgen::JsValue,
    web_sys::{
        Blob,
        BlobPropertyBag,
        ClipboardItem,
    },
};

pub static ICON_SAVE: &str = "icon_clear";
pub static ICON_OPEN: &str = "icon_open";
pub static ICON_SAVEAS: &str = "icon_saveas";
pub static ICON_SAVECOPY: &str = "icon_savecopy";
pub static ICON_CLIPBOARD: &str = "icon_clipboard";
pub(crate) static ICON_CLEAR: &str = "icon_clear";
pub(crate) static ICON_DELETE: &str = "icon_delete";
pub(crate) static ICON_UNDO: &str = "icon_undo";
pub(crate) static ICON_REDO: &str = "icon_redo";
pub(crate) static ICON_PIN: &str = "icon_pin";
pub(crate) static ICON_UNPIN: &str = "icon_unpin";
pub(crate) static ICON_MARK: &str = "icon_mark";
pub(crate) static ICON_UNMARK: &str = "icon_unmark";
pub(crate) static ICON_COPY: &str = "icon_copy";
pub(crate) static ICON_CUT: &str = "icon_cut";
pub(crate) static ICON_PASTE: &str = "icon_paste";
pub(crate) static ICON_FILL: &str = "icon_fill";
pub(crate) static ICON_ADD_LEFT: &str = "icon_add_left";
pub(crate) static ICON_ADD_RIGHT: &str = "icon_add_right";
pub(crate) static ICON_ADD_UP: &str = "icon_add_up";
pub(crate) static ICON_ADD_DOWN: &str = "icon_add_down";
pub(crate) static ICON_SHIFT_UP: &str = "icon_shift_up";
pub(crate) static ICON_SHIFT_DOWN: &str = "icon_shift_down";
pub(crate) static ICON_SORT: &str = "icon_sort";
pub(crate) static ICON_SORT_REV: &str = "icon_sort_rev";
pub(crate) static ICON_TYPE_STRING: &str = "type_string";
pub(crate) static ICON_TYPE_BOOL: &str = "type_bool";
pub(crate) static ICON_TYPE_NUMBER: &str = "type_number";
pub(crate) static ICON_TYPE_NULL: &str = "type_null";
pub(crate) static ICON_TYPE_MISSING: &str = "type_missing";
pub(crate) static ICON_TYPE_JSON: &str = "type_null";
pub(crate) static CLASS_ROOT: &str = "sheetman";
pub(crate) static CLASS_DISABLED: &str = "disabled";
pub(crate) static CLASS_INVALID: &str = "invalid";
pub(crate) static CLASS_FOCUSED: &str = "focused";
pub(crate) static CLASS_MARKED: &str = "marked";
pub(crate) static CLASS_SUBMARKED: &str = "submarked";
pub(crate) static CLASS_EDITING: &str = "editing";
pub(crate) static CLASS_PIN_COL: &str = "pin_col";
pub(crate) static CLASS_PIN_ROW: &str = "pin_row";
pub(crate) static CLASS_HEAD: &str = "head";
pub(crate) static CLASS_TABLE_CONTAINER: &str = "table_container";
pub(crate) static CLASS_TOOLBAR_TAB_BUTTON_FRONT: &str = "toolbar_tab_button_front";
pub(crate) static CLASS_TOOLBAR: &str = "toolbar";
pub(crate) static CLASS_TOOLBAR_TABS: &str = "toolbar_tabs";
pub(crate) static ATTR_VALUE_TYPE: &str = "sheetman_type";
pub(crate) static ATTR_VALUE_TYPE_STR: &str = "str";
pub(crate) static ATTR_VALUE_TYPE_BOOL: &str = "bool";
pub(crate) static ATTR_VALUE_TYPE_JSON: &str = "json";
pub(crate) static ATTR_VALUE_TYPE_NULL: &str = "null";
pub(crate) static ATTR_VALUE_TYPE_MISSING: &str = "missing";
pub(crate) static ATTR_VALUE_TYPE_NUMBER: &str = "number";
pub(crate) static ATTR_TH_CELL_OLD_VALUE: &str = "cell_old";
pub(crate) static ATTR_CONTENTEDITABLE: &str = "contenteditable";
pub(crate) static ATTR_CONTENTEDITABLE_PLAINTEXT: &str = "plaintext-only";
pub(crate) static ATTR_TABINDEX: &str = "tabindex";
static CLIPBOARD_MIME: &str = "text/plain";

pub fn as_blob(data: impl serde::Serialize) -> Blob {
    return Blob::new_with_str_sequence_and_options(&JsValue::from(vec![
        //. .
        JsValue::from(serde_json::to_string_pretty(&data).unwrap())
    ]), &{
        let p = BlobPropertyBag::new();
        p.set_type(CLIPBOARD_MIME);
        p
    }).unwrap();
}

pub fn copy(data: impl serde::Serialize) {
    _ = window().navigator().clipboard().write(&JsValue::from(vec![
        //. .
        ClipboardItem::new_with_record_from_str_to_str_promise(&Object::from_entries(&JsValue::from(vec![
            //. .
            JsValue::from(vec![
                //. .
                JsValue::from(CLIPBOARD_MIME),
                JsValue::from(as_blob(data))
            ])
        ])).unwrap()).unwrap()
    ]));
}
