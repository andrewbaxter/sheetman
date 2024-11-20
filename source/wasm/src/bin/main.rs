use {
    derive_more::derive::{
        Add,
        Sub,
    },
    flowcontrol::{
        shed,
        superif,
    },
    gloo::{
        events::{
            EventListenerOptions,
            EventListenerPhase,
        },
        utils::{
            document,
            window,
        },
    },
    js_sys::Array,
    rooting::{
        el,
        scope_any,
        set_root,
        El,
        ResizeObserver,
        ScopeValue,
        WeakEl,
    },
    serde::{
        Deserialize,
        Serialize,
    },
    serde_json::Number,
    std::{
        cell::{
            Cell,
            OnceCell,
            RefCell,
        },
        cmp::Reverse,
        collections::{
            BTreeSet,
            HashMap,
        },
        mem::swap,
        rc::{
            Rc,
            Weak,
        },
        str::FromStr,
    },
    wasm_bindgen::{
        closure::Closure,
        JsCast,
        JsValue,
    },
    web_sys::{
        console::{
            self,
            log_2,
        },
        Element,
        HtmlElement,
        KeyboardEvent,
        MutationObserver,
        MutationObserverInit,
        MutationRecord,
        Node,
        ResizeObserverBoxOptions,
        ResizeObserverOptions,
    },
    web_time::{
        Duration,
        Instant,
    },
};

static STR_TRUE: &str = "true";
static STR_FALSE: &str = "false";
static ICON_CLEAR: &str = "icon_clear";
static ICON_DELETE: &str = "icon_delete";
static ICON_UNDO: &str = "icon_undo";
static ICON_REDO: &str = "icon_redo";
static ICON_PIN: &str = "icon_pin";
static ICON_UNPIN: &str = "icon_unpin";
static ICON_ADD_LEFT: &str = "icon_add_left";
static ICON_ADD_RIGHT: &str = "icon_add_right";
static ICON_ADD_UP: &str = "icon_add_up";
static ICON_ADD_DOWN: &str = "icon_add_down";
static ICON_SHIFT_UP: &str = "icon_shift_up";
static ICON_SHIFT_DOWN: &str = "icon_shift_down";
static ICON_SORT: &str = "icon_sort";
static ICON_SORT_REV: &str = "icon_sort_rev";
static ICON_TYPE_STRING: &str = "type_string";
static ICON_TYPE_BOOL: &str = "type_bool";
static ICON_TYPE_NUMBER: &str = "type_number";
static ICON_TYPE_NULL: &str = "type_null";
static ICON_TYPE_MISSING: &str = "type_missing";
static ICON_TYPE_JSON: &str = "type_null";
static CLASS_ROOT: &str = "sheetman";
static CLASS_DISABLED: &str = "disabled";
static CLASS_INVALID: &str = "invalid";
static CLASS_SELECTED: &str = "selected";
static CLASS_EDITING: &str = "editing";
static CLASS_PIN_COL: &str = "pin_coll";
static CLASS_PIN_ROW: &str = "pin_row";
static CLASS_BUTTON_PIN: &str = "button_pin";
static CLASS_BUTTON_UNPIN: &str = "button_unpin";
static CLASS_TOOLBAR_TAB_BUTTON_FRONT: &str = "toolbar_tab_button_front";
static ATTR_VALUE_TYPE: &str = "sheetman_type";
static ATTR_VALUE_TYPE_STR: &str = "str";
static ATTR_VALUE_TYPE_BOOL: &str = "bool";
static ATTR_VALUE_TYPE_JSON: &str = "json";
static ATTR_VALUE_TYPE_NULL: &str = "null";
static ATTR_VALUE_TYPE_MISSING: &str = "missing";
static ATTR_VALUE_TYPE_NUMBER: &str = "number";
static ATTR_TH_CELL_OLD_VALUE: &str = "cell_old";
static ATTR_CONTENTEDITABLE: &str = "contenteditable";
static ATTR_CONTENTEDITABLE_PLAINTEXT: &str = "plaintext-only";
static ATTR_TABINDEX: &str = "tabindex";

struct ObsBool_ {
    value: bool,
    cbs: Vec<Weak<RefCell<Box<dyn FnMut(bool)>>>>,
}

#[derive(Clone)]
struct ObsBool(Rc<RefCell<ObsBool_>>);

impl ObsBool {
    fn new(initial: bool) -> Self {
        return Self(Rc::new(RefCell::new(ObsBool_ {
            value: initial,
            cbs: vec![],
        })));
    }

    fn get(&self) -> bool {
        return self.0.as_ref().borrow().value;
    }

    fn set(&self, v: bool) {
        let mut s = self.0.as_ref().borrow_mut();
        if v == s.value {
            return;
        }
        s.value = v;
        let mut cbs = vec![];
        swap(&mut cbs, &mut s.cbs);
        for cb in cbs {
            let Some(cb) = cb.upgrade() else {
                console::log_1(&JsValue::from(format!("obs bool set; skipping dead cb")));
                continue;
            };
            (*cb.as_ref().borrow_mut())(v);
            s.cbs.push(Rc::downgrade(&cb));
        }
    }

    fn listen(&self, mut cb: impl FnMut(bool) + 'static) -> ScopeValue {
        cb(self.0.as_ref().borrow().value);
        let cb = Rc::new(RefCell::new(Box::new(cb) as Box<dyn FnMut(bool)>));
        self.0.as_ref().borrow_mut().cbs.push(Rc::downgrade(&cb));
        return scope_any(cb);
    }
}

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ValueType {
    String,
    Bool,
    Number,
    Json,
    Null,
    Missing,
}

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Add, Sub, Debug)]
struct X(i64);

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Add, Sub, Debug)]
struct Y(i64);

type Coord2 = (X, Y);

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Value {
    type_: ValueType,
    string: String,
}

impl ToString for Value {
    fn to_string(&self) -> String {
        return serde_json::to_string(self).unwrap();
    }
}

impl FromStr for Value {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        return serde_json::from_str(s).map_err(|e| e.to_string());
    }
}

fn get_value(e: &Element) -> Value {
    let text = e.text_content().unwrap();
    let Some(type_) = e.get_attribute(ATTR_VALUE_TYPE) else {
        log_2(&JsValue::from(&format!("Cell missing attribute {}", ATTR_VALUE_TYPE)), &JsValue::from(e));
        return v_str(text);
    };
    let out_type;
    if type_ == ATTR_VALUE_TYPE_STR {
        out_type = ValueType::String;
    } else if type_ == ATTR_VALUE_TYPE_BOOL {
        out_type = ValueType::Bool;
    } else if type_ == ATTR_VALUE_TYPE_NUMBER {
        out_type = ValueType::Number;
    } else if type_ == ATTR_VALUE_TYPE_NULL {
        out_type = ValueType::Null;
    } else if type_ == ATTR_VALUE_TYPE_MISSING {
        out_type = ValueType::Missing;
    } else if type_ == ATTR_VALUE_TYPE_JSON {
        out_type = ValueType::Json;
    } else {
        log_2(&JsValue::from("Cell unknown value type attribute"), &JsValue::from(e));
        return v_str(text);
    }
    return Value {
        type_: out_type,
        string: text,
    };
}

fn to_json(v: &Value) -> Option<serde_json::Value> {
    match v.type_ {
        ValueType::String => return Some(serde_json::Value::String(v.string.clone())),
        ValueType::Bool => {
            if v.string == STR_TRUE {
                return Some(serde_json::Value::Bool(true));
            } else if v.string == STR_FALSE {
                return Some(serde_json::Value::Bool(false));
            } else {
                return None;
            }
        },
        ValueType::Number => {
            if let Ok(n) = v.string.parse::<f64>() {
                return Some(
                    serde_json::Value::Number(
                        serde_json::Number::from_f64(n).unwrap_or(Number::from_f64(0.).unwrap()),
                    ),
                );
            } else {
                return None;
            }
        },
        ValueType::Json => {
            if let Ok(j) = serde_json::from_str::<serde_json::Value>(&v.string) {
                return Some(j);
            } else {
                return None;
            }
        },
        ValueType::Null => {
            return Some(serde_json::Value::Null);
        },
        ValueType::Missing => {
            return Some(serde_json::Value::Null);
        },
    }
}

fn v_str(v: impl AsRef<str>) -> Value {
    return Value {
        type_: ValueType::String,
        string: v.as_ref().to_string(),
    };
}

#[derive(Clone)]
struct ChangeCells {
    cells: HashMap<Coord2, Value>,
}

#[derive(Clone)]
struct ChangeSplice {
    start: Coord2,
    remove: Coord2,
    /// Each element is a column, with entries for all rows not including added rows
    add_columns: Vec<Vec<Value>>,
    /// Each element is a row, with entries for all columns including added columns
    add_rows: Vec<Vec<Value>>,
}

#[derive(Clone)]
enum Change {
    Cells(ChangeCells),
    Splice(ChangeSplice),
}

#[derive(Clone)]
struct ChangeLevel {
    selection: Option<Coord2>,
    changes: Vec<Change>,
}

struct State {
    root: WeakEl,
    table_thr: WeakEl,
    undo: RefCell<Vec<ChangeLevel>>,
    redo: RefCell<Vec<ChangeLevel>>,
    last_change: Cell<Instant>,
    current_undo_level: RefCell<ChangeLevel>,
    always: ObsBool,
    selected: RefCell<Option<Element>>,
    selected_th_or_cell: ObsBool,
    selected_cell: ObsBool,
    selected_col_unpinned: ObsBool,
    selected_row_unpinned: ObsBool,
    has_undo: ObsBool,
    has_redo: ObsBool,
    pin_resize_observer: OnceCell<ResizeObserver>,
}

fn flush_undo(state: &State) {
    let mut current_undo_level = state.current_undo_level.borrow_mut();
    if current_undo_level.changes.is_empty() {
        return;
    }
    state.undo.borrow_mut().push(current_undo_level.clone());
    current_undo_level.changes.clear();
    state.has_undo.set(true);
}

fn push_undo_no_merge(state: &State, sel: Option<Coord2>, change: Change) {
    let mut current_undo_level = state.current_undo_level.borrow_mut();
    if current_undo_level.changes.is_empty() {
        current_undo_level.selection = sel;
    }
    current_undo_level.changes.push(change);
    state.has_undo.set(true);
    state.redo.borrow_mut().clear();
    state.has_redo.set(false);
}

fn push_undo(state: &State, sel: Option<Coord2>, change: Change) {
    let now = Instant::now();
    if now.saturating_duration_since(state.last_change.get()) > Duration::from_millis(1000) {
        flush_undo(state);
    }
    state.last_change.set(now);
    let mut current_undo_level = state.current_undo_level.borrow_mut();
    shed!{
        let Some(last_change) = current_undo_level.changes.last_mut() else {
            break;
        };
        let Change::Cells(last_change) = last_change else {
            break;
        };
        let Change::Cells(change) = change else {
            break;
        };
        for (xy, val) in change.cells {
            if !last_change.cells.contains_key(&xy) {
                last_change.cells.insert(xy, val);
            }
        }
        state.redo.borrow_mut().clear();
        state.has_redo.set(false);
        return;
    }
    drop(current_undo_level);
    push_undo_no_merge(state, sel, change);
}

fn validate_cell(cell: &Element, v: &Value) {
    let ok;
    match v.type_ {
        ValueType::String => ok = true,
        ValueType::Bool => {
            if v.string == STR_TRUE || v.string == STR_FALSE {
                ok = true;
            } else {
                ok = false;
            }
        },
        ValueType::Number => {
            ok = v.string.parse::<f64>().is_ok();
        },
        ValueType::Json => {
            ok = serde_json::from_str::<serde_json::Value>(&v.string).is_ok();
        },
        ValueType::Null => {
            ok = v.string.is_empty();
        },
        ValueType::Missing => {
            ok = v.string.is_empty();
        },
    }
    cell.class_list().toggle_with_force(CLASS_INVALID, !ok).unwrap();
}

fn apply_cell_value(cell: &Element, new_value: &Value) {
    cell.set_text_content(Some(&new_value.string));
    cell.set_attribute(ATTR_VALUE_TYPE, match new_value.type_ {
        ValueType::String => ATTR_VALUE_TYPE_STR,
        ValueType::Bool => ATTR_VALUE_TYPE_BOOL,
        ValueType::Number => ATTR_VALUE_TYPE_NUMBER,
        ValueType::Json => ATTR_VALUE_TYPE_JSON,
        ValueType::Null => ATTR_VALUE_TYPE_NULL,
        ValueType::Missing => ATTR_VALUE_TYPE_MISSING,
    }).unwrap();
    cell.set_attribute(ATTR_TH_CELL_OLD_VALUE, &new_value.to_string()).unwrap();
    validate_cell(cell, &new_value);
}

fn build_th(title: &Value) -> Element {
    let out = document().create_element("th").unwrap();
    out.set_attribute(ATTR_TABINDEX, "0").unwrap();
    apply_cell_value(&out, title);
    return out;
}

fn build_cell(new_value: &Value) -> Element {
    let out = document().create_element("td").unwrap();
    out.set_attribute(ATTR_TABINDEX, "0").unwrap();
    apply_cell_value(&out, new_value);
    return out;
}

fn find_containing_cell(start: &Node) -> Option<Element> {
    let mut at = start.clone();
    loop {
        shed!{
            if at.node_type() != Node::ELEMENT_NODE {
                break;
            }
            let el = at.dyn_ref::<Element>().unwrap();
            let el_tag = el.tag_name().to_ascii_lowercase();
            if el_tag == "table" || el_tag == "tr" {
                return None;
            }
            if el_tag == "th" || el_tag == "td" {
                return Some(el.clone());
            }
        };
        let Some(at1) = at.parent_node() else {
            return None;
        };
        at = at1;
    };
}

fn apply(state: &Rc<State>, change: Change) -> (Option<Coord2>, Change) {
    match change {
        Change::Cells(change) => {
            let mut out = HashMap::new();
            for (xy, new_value) in change.cells {
                let cell = get_cell(state, xy).unwrap();
                if xy.1 == Y(0) {
                    if new_value.type_ != ValueType::String {
                        panic!();
                    }
                    out.insert(xy, v_str(&cell.text_content().unwrap()));
                    apply_cell_value(&cell, &new_value);
                } else {
                    out.insert(xy, get_value(&cell));
                    apply_cell_value(&cell, &new_value);
                }
            }
            return (None, Change::Cells(ChangeCells { cells: out }));
        },
        Change::Splice(change) => {
            let rows_parent = state.table_thr.upgrade().unwrap().raw().parent_element().unwrap();
            let rows = rows_parent.children();
            let rev_change_remove = (X(change.add_columns.len() as i64), Y(change.add_rows.len() as i64));
            let old_selection = find_selection(state).map(|s| s.0);
            let initial_column_count = column_count(state);
            let initial_row_count = Y(rows.length() as i64);
            console::log_1(
                &JsValue::from(
                    format!(
                        "--- Splice; start {:?}, remove {:?}, add cols {}, add rows {}",
                        change.start,
                        change.remove,
                        change.add_columns.len(),
                        change.add_rows.len()
                    ),
                ),
            );

            // Remove rows
            let mut rev_add_rows = vec![];
            for _ in change.start.1.0 .. change.remove.1.0 {
                let row = rows.item(change.start.1.0 as u32).unwrap();
                console::log_1(&JsValue::from(format!("remove row at {}", change.start.1.0)));
                let row_children = row.children();
                let mut rev_add_row = vec![];
                for i in 0 .. row_children.length() {
                    rev_add_row.push(get_value(&row_children.item(i).unwrap()));
                }
                row.remove();
                rev_add_rows.push(rev_add_row);
            }

            // .
            let mut rev_add_columns = vec![];
            for _ in 0 .. change.remove.0.0 {
                rev_add_columns.push(vec![]);
            }
            for y in 0 .. rows.length() {
                let row = rows.item(y).unwrap();
                let cells = row.children();

                // Remove columns
                for i in 0 .. change.remove.0.0 {
                    let cell = cells.item(change.start.0.0 as u32).unwrap();
                    rev_add_columns.get_mut(i as usize).unwrap().push(get_value(&cell));
                    cell.remove();
                    console::log_1(&JsValue::from(format!("remove col cell at y {} i {}", y, i)));
                }

                // Add new columns
                let add_cells_before = cells.item(change.start.0.0 as u32).map(|v| v.dyn_into::<Node>().unwrap());
                for col_values in &change.add_columns {
                    let cell_value = col_values.get(y as usize).unwrap();
                    let cell;
                    if y == 0 {
                        cell = build_th(&cell_value);
                    } else {
                        cell = build_cell(&cell_value);
                    }
                    row
                        .insert_before(
                            &cell,
                            add_cells_before.clone().map(|v| v.dyn_into::<Node>().unwrap()).as_ref(),
                        )
                        .unwrap();
                    console::log_1(&JsValue::from(format!("add col cell")));
                }
            }

            // Add rows
            let add_rows_before = rows.item(change.start.1.0 as u32 + 1).map(|r| r.dyn_into::<Node>().unwrap());
            for row_values in change.add_rows {
                let new_row = document().create_element("tr").unwrap();
                for cell_value in row_values {
                    new_row.append_child(&build_cell(&cell_value)).unwrap();
                }
                rows_parent.insert_before(&new_row, add_rows_before.as_ref()).unwrap();
            }

            // Adjust selection
            let new_selection;
            if let Some((old_x, old_y)) = old_selection {
                let new_x;
                if old_x < change.start.0 {
                    new_x = old_x;
                } else {
                    if change.start.0 == initial_column_count {
                        new_x = change.start.0 - X(1);
                    } else {
                        new_x = change.start.0;
                    }
                }
                let new_y;
                if old_y < change.start.1 {
                    new_y = old_y;
                } else {
                    if change.start.1 == initial_row_count {
                        new_y = change.start.1 - Y(1);
                    } else {
                        new_y = change.start.1;
                    }
                }
                let new_selection1 = (new_x, new_y);
                if new_selection1 != (old_x, old_y) {
                    new_selection = Some(new_selection1);
                } else {
                    new_selection = None;
                }
            } else {
                new_selection = None;
            }

            // Return reverse change
            return (new_selection, Change::Splice(ChangeSplice {
                start: change.start,
                remove: rev_change_remove,
                add_columns: rev_add_columns,
                add_rows: rev_add_rows,
            }));
        },
    }
}

fn select(state: &Rc<State>, new_sel: Coord2) {
    let cell = get_cell(state, new_sel).unwrap().dyn_into::<HtmlElement>().unwrap();
    cell.focus().unwrap();
    register_select(state, &cell);
}

fn column_count(state: &State) -> X {
    return X(state.table_thr.upgrade().unwrap().raw().children().length() as i64);
}

fn row_count(state: &State) -> Y {
    return Y(state.table_thr.upgrade().unwrap().raw().parent_element().unwrap().children().length() as i64 - 1);
}

fn el_toolbar_tab_button(title: impl AsRef<str>, mut cb: impl FnMut(&El) + 'static) -> El {
    let out = el("button");
    out.ref_text(title.as_ref());
    out.ref_on("click", {
        let e = out.clone();
        move |_| {
            cb(&e);
        }
    });
    return out;
}

fn el_toolbar_button(text: impl AsRef<str>, icon: &str, enabled: &ObsBool, mut cb: impl FnMut(&El) + 'static) -> El {
    let out = el("button");
    out.ref_text(text.as_ref());
    out.ref_classes(&[icon]);
    out.ref_on("click", {
        let e = out.clone();
        let enabled = enabled.clone();
        move |_| {
            if !enabled.get() {
                return;
            }
            cb(&e);
        }
    });
    out.ref_own(|e| enabled.listen({
        let e = e.weak();
        move |enabled| {
            let Some(e) = e.upgrade() else {
                return;
            };
            e.ref_modify_classes(&[(CLASS_DISABLED, !enabled)]);
        }
    }));
    return out;
}

fn el_toolbar_button_2state(
    // Text, css class; 0 = true, 1 = false state
    state_values: [(&'static str, &'static str); 2],
    enabled: &ObsBool,
    icon_state: &ObsBool,
    mut cb: impl FnMut(&El) + 'static,
) -> El {
    let out = el("button");
    out.ref_on("click", {
        let e = out.clone();
        let enabled = enabled.clone();
        move |_| {
            if !enabled.get() {
                return;
            }
            cb(&e);
        }
    });
    out.ref_own(|e| enabled.listen({
        let e = e.weak();
        move |enabled| {
            let Some(e) = e.upgrade() else {
                return;
            };
            e.ref_modify_classes(&[(CLASS_DISABLED, !enabled)]);
        }
    }));
    out.ref_own(|e| icon_state.listen({
        let e = e.weak();
        move |icon_state| {
            let Some(e) = e.upgrade() else {
                return;
            };
            let index;
            if icon_state {
                index = 0;
            } else {
                index = 1;
            }
            let other_index = 1 - index;
            e.ref_text(state_values[index].0);
            e.ref_modify_classes(&[(state_values[index].1, true), (state_values[other_index].1, false)]);
        }
    }));
    return out;
}

fn update_column_pin_from(state: &Rc<State>, cell: &Element) {
    let start_row_pinned = cell.parent_element().unwrap().get_elements_by_class_name(CLASS_PIN_COL);
    let pin_x;
    shed!{
        'found _;
        for i in 0 .. start_row_pinned.length() {
            if &start_row_pinned.item(i).unwrap() == cell {
                pin_x = i as u32;
                break 'found;
            }
        }
        panic!();
    };
    let cell = cell.dyn_ref::<HtmlElement>().unwrap();
    let start_right = cell.get_bounding_client_rect().right();
    for row in get_rows(&state) {
        let row_pinned = row.get_elements_by_class_name(CLASS_PIN_COL);
        let mut prev_right = start_right;
        for i in pin_x + 1 .. row_pinned.length() {
            let cell = row_pinned.item(i).unwrap().dyn_into::<HtmlElement>().unwrap();
            cell.style().set_property("inset-left", &format!("{}px", prev_right)).unwrap();
            prev_right = cell.get_bounding_client_rect().right();
        }
    }
}

fn update_row_pin_from(cell: &Element) {
    let row = cell.parent_element().unwrap();
    let pinned_rows = row.parent_element().unwrap().get_elements_by_class_name(CLASS_PIN_ROW);
    let pin_y;
    shed!{
        'found _;
        for i in 0 .. pinned_rows.length() {
            if pinned_rows.item(i).unwrap() == row {
                pin_y = i as u32;
                break 'found;
            }
        }
        panic!();
    };
    let cell = cell.dyn_ref::<HtmlElement>().unwrap();
    let mut prev_bottom = cell.get_bounding_client_rect().bottom();
    for i in pin_y + 1 .. pinned_rows.length() {
        let row = pinned_rows.item(i).unwrap().dyn_into::<HtmlElement>().unwrap();
        row.style().set_property("inset-top", &format!("{}px", prev_bottom)).unwrap();
        prev_bottom = row.get_bounding_client_rect().bottom();
    }
}

fn get_column(state: &State, col: X) -> Vec<Element> {
    let mut out = vec![];
    let mut at = state.table_thr.upgrade().unwrap().raw();
    while let Some(row) = at.next_element_sibling() {
        out.push(row.children().item(col.0 as u32).unwrap());
        at = row;
    }
    return out;
}

fn get_rows(state: &State) -> Vec<Element> {
    let mut out = vec![];
    let mut at = state.table_thr.upgrade().unwrap().raw();
    while let Some(row) = at.next_element_sibling() {
        out.push(row.clone());
        at = row;
    }
    return out;
}

fn get_row(state: &State, row: Y) -> Vec<Element> {
    let mut out = vec![];
    let row =
        state
            .table_thr
            .upgrade()
            .unwrap()
            .raw()
            .parent_element()
            .unwrap()
            .children()
            .item((row.0) as u32)
            .unwrap()
            .children();
    for i in 0 .. row.length() {
        out.push(row.item(i).unwrap());
    }
    return out;
}

fn get_cell(state: &State, coord2: Coord2) -> Option<Element> {
    let Some(row) =
        state.table_thr.upgrade().unwrap().raw().parent_element().unwrap().children().item((coord2.1.0) as u32) else {
            return None;
        };
    return row.children().item((coord2.0.0) as u32);
}

fn index_in_parent(e: &Element) -> usize {
    let sibs = e.parent_element().unwrap().children();
    for i in 0 .. sibs.length() {
        if &sibs.item(i).unwrap() == e {
            return i as usize;
        }
    }
    panic!();
}

fn start_editing_cell(state: &Rc<State>, cell: &Element) {
    let v = get_value(cell);
    match v.type_ {
        ValueType::Bool => {
            let new_string;
            if v.string == STR_TRUE {
                new_string = STR_FALSE.to_string();
            } else {
                new_string = STR_TRUE.to_string();
            }
            let xy = get_xy(cell);
            let (new_sel, rev_change) = apply(state, Change::Cells(ChangeCells { cells: [(xy, Value {
                type_: v.type_,
                string: new_string,
            })].into_iter().collect() }));
            if new_sel.is_some() {
                panic!();
            }
            push_undo(state, Some(xy), rev_change);
        },
        ValueType::String | ValueType::Number | ValueType::Json => {
            cell.set_attribute(ATTR_CONTENTEDITABLE, ATTR_CONTENTEDITABLE_PLAINTEXT).unwrap();
            cell.class_list().add_1(CLASS_EDITING).unwrap();
            {
                let sel = window().get_selection().unwrap().unwrap();
                sel.remove_all_ranges().unwrap();
                let range = document().create_range().unwrap();
                let child_nodes = cell.child_nodes();
                if child_nodes.length() == 0 {
                    range.set_start(cell, 0).unwrap();
                    range.set_end(cell, 0).unwrap();
                } else {
                    let text_node = child_nodes.item(0).unwrap();
                    let end = text_node.text_content().unwrap().len() as u32;
                    range.set_start(&text_node, end as u32).unwrap();
                    range.set_end(&text_node, end as u32).unwrap();
                }
                sel.add_range(&range).unwrap();
            }
        },
        ValueType::Null => return,
        ValueType::Missing => return,
    }
}

fn stop_editing_cell(cell: &Element) {
    cell.remove_attribute(ATTR_CONTENTEDITABLE).unwrap();
    cell.class_list().remove_1(CLASS_EDITING).unwrap();
}

fn replace_toolbar_tab_front(state: &Rc<State>, button: &El) {
    let old_tab_buttons =
        state.root.upgrade().unwrap().raw().get_elements_by_class_name(CLASS_TOOLBAR_TAB_BUTTON_FRONT);
    for i in 0 .. old_tab_buttons.length() {
        old_tab_buttons.item(i).unwrap().class_list().remove_1(CLASS_TOOLBAR_TAB_BUTTON_FRONT).unwrap();
    }
    button.ref_classes(&[CLASS_TOOLBAR_TAB_BUTTON_FRONT]);
}

fn get_xy(cell: &Element) -> Coord2 {
    return (X(index_in_parent(&cell) as i64), Y(index_in_parent(&cell.parent_element().unwrap()) as i64));
}

fn find_selection(state: &Rc<State>) -> Option<(Coord2, Element)> {
    let selected =
        state
            .table_thr
            .upgrade()
            .unwrap()
            .raw()
            .parent_element()
            .unwrap()
            .get_elements_by_class_name(CLASS_SELECTED);
    let Some(cell) = selected.item(0) else {
        return None;
    };
    return Some((get_xy(&cell), cell));
}

fn act_add_left(state: &Rc<State>) {
    let Some((sel_xy, _)) = find_selection(state) else {
        return;
    };
    let change = Change::Splice(ChangeSplice {
        start: (sel_xy.0, Y(0)),
        remove: (X(0), Y(0)),
        add_columns: vec![(0 .. 1 + row_count(&state).0).map(|_| v_str("")).collect()],
        add_rows: vec![],
    });
    flush_undo(&state);
    push_undo(&state, Some(sel_xy), apply(&state, change).1);
    flush_undo(&state);
    select(&state, sel_xy);
}

fn act_add_right(state: &Rc<State>) {
    let Some((sel_xy, _)) = find_selection(state) else {
        return;
    };
    let change = Change::Splice(ChangeSplice {
        start: (sel_xy.0 + X(1), Y(0)),
        remove: (X(0), Y(0)),
        add_columns: vec![(0 .. 1 + row_count(&state).0).map(|_| v_str("")).collect()],
        add_rows: vec![],
    });
    flush_undo(&state);
    push_undo(&state, Some(sel_xy), apply(&state, change).1);
    flush_undo(&state);
    select(&state, (sel_xy.0 + X(1), sel_xy.1));
}

fn act_add_up(state: &Rc<State>) {
    let Some((sel_xy, _)) = find_selection(state) else {
        return;
    };
    if sel_xy.1 == Y(0) {
        return;
    }
    let base_row = get_row(&state, sel_xy.1);
    let change = Change::Splice(ChangeSplice {
        start: (X(0), sel_xy.1),
        remove: (X(0), Y(0)),
        add_columns: vec![],
        add_rows: vec![base_row.into_iter().map(|base_cell| Value {
            type_: get_value(&base_cell).type_,
            string: "".to_string(),
        }).collect()],
    });
    flush_undo(&state);
    push_undo(&state, Some(sel_xy), apply(&state, change).1);
    flush_undo(&state);
    select(&state, sel_xy);
}

fn act_add_down(state: &Rc<State>) {
    let Some((sel_xy, _)) = find_selection(state) else {
        return;
    };
    if sel_xy.1 == Y(0) {
        return;
    }
    let base_row = get_row(&state, sel_xy.1);
    let change = Change::Splice(ChangeSplice {
        start: (X(0), sel_xy.1 + Y(1)),
        remove: (X(0), Y(0)),
        add_columns: vec![],
        add_rows: vec![base_row.into_iter().map(|base_cell| Value {
            type_: get_value(&base_cell).type_,
            string: "".to_string(),
        }).collect()],
    });
    flush_undo(&state);
    push_undo(&state, Some(sel_xy), apply(&state, change).1);
    flush_undo(&state);
    select(&state, (sel_xy.0, sel_xy.1 + Y(1)));
}

fn act_undo(state: &Rc<State>) {
    flush_undo(&state);
    let level = state.undo.borrow_mut().pop();
    if let Some(mut level) = level {
        let mut rev_level = ChangeLevel {
            selection: find_selection(state).map(|s| s.0),
            changes: vec![],
        };
        let mut last_apply_sel = None;
        while let Some(change) = level.changes.pop() {
            let (apply_sel, rev_change) = apply(&state, change);
            rev_level.changes.push(rev_change);
            last_apply_sel = apply_sel;
        }
        state.redo.borrow_mut().push(rev_level);
        state.has_redo.set(true);
        if state.undo.borrow().is_empty() {
            state.has_undo.set(false);
        }
        if let Some(sel) = level.selection.or(last_apply_sel) {
            select(&state, sel);
        }
    }
}

fn act_redo(state: &Rc<State>) {
    flush_undo(&state);
    let level = state.redo.borrow_mut().pop();
    if let Some(mut level) = level {
        let mut rev_level = ChangeLevel {
            selection: find_selection(state).map(|s| s.0),
            changes: vec![],
        };
        let mut last_apply_sel = None;
        while let Some(change) = level.changes.pop() {
            let (apply_sel, rev_change) = apply(&state, change);
            rev_level.changes.push(rev_change);
            last_apply_sel = apply_sel;
        }
        state.undo.borrow_mut().push(rev_level);
        state.has_undo.set(true);
        if let Some(sel) = level.selection.or(last_apply_sel) {
            select(&state, sel);
        }
    }
}

fn build_toolbar_sheet(state: &Rc<State>, button: &El, toolbar: &El) {
    replace_toolbar_tab_front(&state, &button);
    toolbar.ref_clear();
    toolbar.ref_extend(vec![
        //. .
        el_toolbar_button("Clear", ICON_CLEAR, &state.always, {
            let state = state.clone();
            move |_| {
                let (new_sel, rev_change) = apply(&state, Change::Splice(ChangeSplice {
                    start: (X(0), Y(0)),
                    remove: (column_count(&state), row_count(&state)),
                    add_columns: vec![vec![v_str(""), v_str("")]],
                    add_rows: vec![],
                }));
                push_undo(&state, find_selection(&state).map(|s| s.0), rev_change);
                if let Some(new_sel) = new_sel {
                    select(&state, new_sel);
                }
            }
        }),
        el_toolbar_button("Undo", ICON_UNDO, &state.has_undo, {
            let state = state.clone();
            move |_| act_undo(&state)
        }),
        el_toolbar_button("Redo", ICON_REDO, &state.has_redo, {
            let state = state.clone();
            move |_| act_redo(&state)
        })
    ]);
}

fn build_toolbar_column(state: &Rc<State>, button: &El, toolbar: &El) {
    replace_toolbar_tab_front(&state, &button);
    toolbar.ref_clear();
    toolbar.ref_extend(vec![
        //. .
        el_toolbar_button_2state([("Pin", ICON_PIN), ("Unpin", ICON_UNPIN)], &state.always, &state.selected_col_unpinned, {
            let state = state.clone();
            move |e| {
                let Some((sel_xy, _)) = find_selection(&state) else {
                    return;
                };
                let mut cells = get_column(&state, sel_xy.0).into_iter();
                let Some(first_cell) = cells.next() else {
                    panic!();
                };
                let first_classes = first_cell.class_list();
                let unpinned = !first_classes.contains(CLASS_PIN_COL);
                if !unpinned {
                    first_classes.remove_1(CLASS_PIN_COL).unwrap();
                    state.pin_resize_observer.get().unwrap().0.js_resize_observer.unobserve(&first_cell);
                    for cell in cells {
                        cell.class_list().remove_1(CLASS_PIN_COL).unwrap();
                        state.pin_resize_observer.get().unwrap().0.js_resize_observer.unobserve(&cell);
                    }
                    e.ref_text("Pin");
                    e.ref_modify_classes(&[(CLASS_BUTTON_PIN, true), (CLASS_BUTTON_UNPIN, false)]);
                } else {
                    first_classes.add_1(CLASS_PIN_COL).unwrap();
                    let obs_opts = ResizeObserverOptions::new();
                    obs_opts.set_box(ResizeObserverBoxOptions::BorderBox);
                    state
                        .pin_resize_observer
                        .get()
                        .unwrap()
                        .0
                        .js_resize_observer
                        .observe_with_options(&first_cell, &obs_opts);
                    for cell in cells {
                        cell.class_list().add_1(CLASS_PIN_COL).unwrap();
                        state
                            .pin_resize_observer
                            .get()
                            .unwrap()
                            .0
                            .js_resize_observer
                            .observe_with_options(&cell, &obs_opts);
                    }
                    e.ref_text("Unpin");
                    e.ref_modify_classes(&[(CLASS_BUTTON_PIN, false), (CLASS_BUTTON_UNPIN, true)]);
                    update_column_pin_from(&state, &first_cell);
                }
                state.selected_col_unpinned.set(unpinned);
            }
        }),
        el_toolbar_button("Add left", ICON_ADD_LEFT, &state.always, {
            let state = state.clone();
            move |_| act_add_left(&state)
        }),
        el_toolbar_button("Add right", ICON_ADD_RIGHT, &state.always, {
            let state = state.clone();
            move |_| act_add_right(&state)
        }),
        el_toolbar_button("Delete", ICON_DELETE, &state.always, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, _)) = find_selection(&state) else {
                    return;
                };
                let col_count = column_count(&state);
                let change = Change::Splice(ChangeSplice {
                    start: (sel_xy.0, Y(0)),
                    remove: (X(1), Y(0)),
                    add_columns: if col_count > X(1) {
                        vec![]
                    } else {
                        vec![(0 .. 1 + row_count(&state).0).map(|_| v_str("")).collect()]
                    },
                    add_rows: vec![],
                });
                let (new_sel, rev_change) = apply(&state, change);
                flush_undo(&state);
                push_undo(&state, Some(sel_xy), rev_change);
                flush_undo(&state);
                if let Some(new_sel) = new_sel {
                    select(&state, new_sel);
                }
            }
        }),
        el_toolbar_button("Sort", ICON_SORT, &state.always, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, _)) = find_selection(&state) else {
                    return;
                };
                let mut rows1 = vec![];
                for row in get_rows(&state) {
                    let mut cells = vec![];
                    let el_cells = row.children();
                    for i in 0 .. el_cells.length() {
                        let el_cell = el_cells.item(i).unwrap();
                        cells.push(get_value(&el_cell));
                    }
                    rows1.push(cells);
                }
                let col_idx = sel_xy.0.0;
                rows1.sort_by_cached_key(|r| r.get(col_idx as usize).unwrap().clone());
                let mut change = ChangeCells { cells: HashMap::new() };
                for (row_idx, row) in rows1.into_iter().enumerate() {
                    for (col_idx, cell) in row.into_iter().enumerate() {
                        change.cells.insert((X(row_idx as i64), Y(col_idx as i64)), cell);
                    }
                }
                flush_undo(&state);
                push_undo(&state, Some(sel_xy), apply(&state, Change::Cells(change)).1);
                flush_undo(&state);
            }
        }),
        el_toolbar_button("Sort reverse", ICON_SORT_REV, &state.always, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, _)) = find_selection(&state) else {
                    return;
                };
                let mut rows1 = vec![];
                for row in get_rows(&state) {
                    let mut cells = vec![];
                    let el_cells = row.children();
                    for i in 0 .. el_cells.length() {
                        let el_cell = el_cells.item(i).unwrap();
                        cells.push(get_value(&el_cell));
                    }
                    rows1.push(cells);
                }
                let col_idx = sel_xy.0.0;
                rows1.sort_by_cached_key(|r| Reverse(r.get(col_idx as usize).unwrap().clone()));
                let mut change = ChangeCells { cells: HashMap::new() };
                for (row_idx, row) in rows1.into_iter().enumerate() {
                    for (col_idx, cell) in row.into_iter().enumerate() {
                        change.cells.insert((X(row_idx as i64), Y(col_idx as i64)), cell);
                    }
                }
                flush_undo(&state);
                push_undo(&state, Some(sel_xy), apply(&state, Change::Cells(change)).1);
                flush_undo(&state);
            }
        })
    ]);
}

fn build_toolbar_row(state: &Rc<State>, button: &El, toolbar: &El) {
    replace_toolbar_tab_front(&state, &button);
    toolbar.ref_clear();
    toolbar.ref_extend(vec![
        //. .
        el_toolbar_button_2state(
            [("Pin", ICON_PIN), ("Unpin", ICON_UNPIN)],
            &state.selected_cell,
            &state.selected_row_unpinned,
            {
                let state = state.clone();
                move |e| {
                    let Some((sel_xy, _)) = find_selection(&state) else {
                        return;
                    };
                    let mut cells = get_row(&state, sel_xy.1).into_iter();
                    let Some(first_cell) = cells.next() else {
                        panic!();
                    };
                    let first_classes = first_cell.class_list();
                    let unpinned = !first_classes.contains(CLASS_PIN_ROW);
                    if !unpinned {
                        first_classes.remove_1(CLASS_PIN_ROW).unwrap();
                        state.pin_resize_observer.get().unwrap().0.js_resize_observer.unobserve(&first_cell);
                        for cell in cells {
                            cell.class_list().remove_1(CLASS_PIN_ROW).unwrap();
                            state.pin_resize_observer.get().unwrap().0.js_resize_observer.unobserve(&cell);
                        }
                        e.ref_text("Pin");
                        e.ref_modify_classes(&[(CLASS_BUTTON_PIN, true), (CLASS_BUTTON_UNPIN, false)]);
                    } else {
                        first_classes.add_1(CLASS_PIN_ROW).unwrap();
                        let obs_opts = ResizeObserverOptions::new();
                        obs_opts.set_box(ResizeObserverBoxOptions::BorderBox);
                        state
                            .pin_resize_observer
                            .get()
                            .unwrap()
                            .0
                            .js_resize_observer
                            .observe_with_options(&first_cell, &obs_opts);
                        for cell in cells {
                            cell.class_list().add_1(CLASS_PIN_ROW).unwrap();
                            state
                                .pin_resize_observer
                                .get()
                                .unwrap()
                                .0
                                .js_resize_observer
                                .observe_with_options(&cell, &obs_opts);
                        }
                        e.ref_text("Unpin");
                        e.ref_modify_classes(&[(CLASS_BUTTON_PIN, false), (CLASS_BUTTON_UNPIN, true)]);
                        update_row_pin_from(&first_cell);
                    }
                    state.selected_col_unpinned.set(unpinned);
                }
            },
        ),
        el_toolbar_button("Add above", ICON_ADD_UP, &state.selected_cell, {
            let state = state.clone();
            move |_| act_add_up(&state)
        }),
        el_toolbar_button("Add below", ICON_ADD_DOWN, &state.selected_cell, {
            let state = state.clone();
            move |_| act_add_down(&state)
        }),
        el_toolbar_button("Delete", ICON_DELETE, &state.selected_cell, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, _)) = find_selection(&state) else {
                    return;
                };
                let change = Change::Splice(ChangeSplice {
                    start: (X(0), sel_xy.1),
                    remove: (X(0), Y(1)),
                    add_columns: vec![],
                    add_rows: vec![],
                });
                let (new_sel, rev_change) = apply(&state, change);
                flush_undo(&state);
                push_undo(&state, Some(sel_xy), rev_change);
                flush_undo(&state);
                if let Some(new_sel) = new_sel {
                    select(&state, new_sel);
                }
            }
        }),
        el_toolbar_button("Shift up", ICON_SHIFT_UP, &state.selected_cell, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, _)) = find_selection(&state) else {
                    return;
                };
                if sel_xy.1 <= Y(1) {
                    return;
                }
                flush_undo(&state);
                let change = Change::Splice(ChangeSplice {
                    start: (X(0), sel_xy.1),
                    remove: (X(0), Y(1)),
                    add_rows: vec![],
                    add_columns: vec![],
                });
                let rev_change = apply(&state, change.clone()).1;
                let Change::Splice(rev_change_splice) = &rev_change else {
                    panic!();
                };
                push_undo_no_merge(&state, Some(sel_xy), rev_change.clone());
                let rev_change = Change::Splice(ChangeSplice {
                    start: (X(0), sel_xy.1 - Y(1)),
                    remove: (X(0), Y(0)),
                    add_rows: rev_change_splice.add_rows.clone(),
                    add_columns: vec![],
                });
                push_undo_no_merge(&state, Some(sel_xy), rev_change.clone());
                flush_undo(&state);
                select(&state, (sel_xy.0, sel_xy.1 - Y(1)));
            }
        }),
        el_toolbar_button("Shift down", ICON_SHIFT_DOWN, &state.selected_cell, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, _)) = find_selection(&state) else {
                    return;
                };
                if sel_xy.1 == row_count(&state) {
                    return;
                }
                flush_undo(&state);
                let change = Change::Splice(ChangeSplice {
                    start: (X(0), sel_xy.1),
                    remove: (X(0), Y(1)),
                    add_rows: vec![],
                    add_columns: vec![],
                });
                let rev_change = apply(&state, change.clone()).1;
                let Change::Splice(rev_change_splice) = &rev_change else {
                    panic!();
                };
                push_undo_no_merge(&state, Some(sel_xy), rev_change.clone());
                let rev_change = Change::Splice(ChangeSplice {
                    start: (X(0), sel_xy.1 + Y(1)),
                    remove: (X(0), Y(0)),
                    add_rows: rev_change_splice.add_rows.clone(),
                    add_columns: vec![],
                });
                push_undo_no_merge(&state, Some(sel_xy), rev_change.clone());
                flush_undo(&state);
                select(&state, (sel_xy.0, sel_xy.1 + Y(1)));
            }
        })
    ]);
}

fn build_toolbar_cell(state: &Rc<State>, button: &El, toolbar: &El) {
    replace_toolbar_tab_front(&state, &button);
    toolbar.ref_clear();
    toolbar.ref_extend(vec![
        //. .
        el_toolbar_button("String", ICON_TYPE_STRING, &state.selected_cell, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, sel)) = find_selection(&state) else {
                    return;
                };
                if sel_xy.1 == Y(0) {
                    return;
                }
                let value = get_value(&sel);
                let new_string;
                shed!{
                    match value.type_ {
                        ValueType::String => {
                            return;
                        },
                        ValueType::Json => {
                            if let Some(serde_json::Value::String(v)) = to_json(&value) {
                                new_string = v;
                                break;
                            }
                        },
                        _ => { },
                    }
                    new_string = value.string;
                };
                let cell_change = (sel_xy, Value {
                    type_: ValueType::String,
                    string: new_string,
                });
                let (new_sel, rev_change) =
                    apply(&state, Change::Cells(ChangeCells { cells: [cell_change].into_iter().collect() }));
                if new_sel.is_some() {
                    panic!();
                }
                push_undo(&state, Some(sel_xy), rev_change);
            }
        }),
        el_toolbar_button("Number", ICON_TYPE_NUMBER, &state.selected_cell, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, sel)) = find_selection(&state) else {
                    return;
                };
                if sel_xy.1 == Y(0) {
                    return;
                }
                let value = get_value(&sel);
                let new_string;
                shed!{
                    match value.type_ {
                        ValueType::Number => {
                            return;
                        },
                        ValueType::String => {
                            if value.string.parse::<f64>().is_ok() {
                                new_string = value.string;
                                break;
                            }
                        },
                        ValueType::Json => {
                            if let Some(serde_json::Value::Number(v)) = to_json(&value) {
                                new_string = v.to_string();
                                break;
                            }
                        },
                        _ => { },
                    }
                    new_string = "".to_string();
                };
                let cell_change = (sel_xy, Value {
                    type_: ValueType::Number,
                    string: new_string,
                });
                let (new_sel, rev_change) =
                    apply(&state, Change::Cells(ChangeCells { cells: [cell_change].into_iter().collect() }));
                if new_sel.is_some() {
                    panic!();
                }
                push_undo(&state, Some(sel_xy), rev_change);
            }
        }),
        el_toolbar_button("Bool", ICON_TYPE_BOOL, &state.selected_cell, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, sel)) = find_selection(&state) else {
                    return;
                };
                if sel_xy.1 == Y(0) {
                    return;
                }
                let value = get_value(&sel);
                let new_string;
                shed!{
                    match value.type_ {
                        ValueType::String => {
                            if value.string == STR_TRUE || value.string == STR_FALSE {
                                new_string = value.string;
                                break;
                            }
                        },
                        ValueType::Bool => {
                            return;
                        },
                        ValueType::Json => {
                            if let Some(serde_json::Value::Bool(v)) = to_json(&value) {
                                new_string = v.to_string();
                                break;
                            }
                        },
                        _ => { },
                    }
                    new_string = STR_FALSE.to_string();
                };
                let cell_change = (sel_xy, Value {
                    type_: ValueType::Bool,
                    string: new_string,
                });
                let (new_sel, rev_change) =
                    apply(&state, Change::Cells(ChangeCells { cells: [cell_change].into_iter().collect() }));
                if new_sel.is_some() {
                    panic!();
                }
                push_undo(&state, Some(sel_xy), rev_change);
            }
        }),
        el_toolbar_button("Json", ICON_TYPE_JSON, &state.selected_cell, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, sel)) = find_selection(&state) else {
                    return;
                };
                if sel_xy.1 == Y(0) {
                    return;
                }
                let value = get_value(&sel);
                let new_string;
                if let Some(json) = to_json(&value) {
                    new_string = serde_json::to_string(&json).unwrap();
                } else {
                    new_string = value.string;
                };
                let cell_change = (sel_xy, Value {
                    type_: ValueType::Json,
                    string: new_string,
                });
                let (new_sel, rev_change) =
                    apply(&state, Change::Cells(ChangeCells { cells: [cell_change].into_iter().collect() }));
                if new_sel.is_some() {
                    panic!();
                }
                push_undo(&state, Some(sel_xy), rev_change);
            }
        }),
        el_toolbar_button("Null", ICON_TYPE_NULL, &state.selected_cell, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, _)) = find_selection(&state) else {
                    return;
                };
                if sel_xy.1 == Y(0) {
                    return;
                }
                let cell_change = (sel_xy, Value {
                    type_: ValueType::Null,
                    string: "".to_string(),
                });
                let (new_sel, rev_change) =
                    apply(&state, Change::Cells(ChangeCells { cells: [cell_change].into_iter().collect() }));
                if new_sel.is_some() {
                    panic!();
                }
                push_undo(&state, Some(sel_xy), rev_change);
            }
        }),
        el_toolbar_button("Missing", ICON_TYPE_MISSING, &state.selected_cell, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, _)) = find_selection(&state) else {
                    return;
                };
                if sel_xy.1 == Y(0) {
                    return;
                }
                let cell_change = (sel_xy, Value {
                    type_: ValueType::Missing,
                    string: "".to_string(),
                });
                let (new_sel, rev_change) =
                    apply(&state, Change::Cells(ChangeCells { cells: [cell_change].into_iter().collect() }));
                if new_sel.is_some() {
                    panic!();
                }
                push_undo(&state, Some(sel_xy), rev_change);
            }
        })
    ]);
}

fn register_select(state: &Rc<State>, cell: &Element) {
    let classes = cell.class_list();
    if !classes.contains(CLASS_SELECTED) {
        // Deselect old
        let old_selected =
            state
                .table_thr
                .upgrade()
                .unwrap()
                .raw()
                .parent_element()
                .unwrap()
                .get_elements_by_class_name(CLASS_SELECTED);
        for i in 0 .. old_selected.length() {
            let cell = old_selected.item(i).unwrap();
            cell.class_list().remove_1(CLASS_SELECTED).unwrap();
            stop_editing_cell(&cell);
        }

        // Select new
        classes.add_1(CLASS_SELECTED).unwrap();
        let xy = get_xy(&cell);
        *state.selected.borrow_mut() = Some(cell.clone());
        if xy.1 == Y(0) {
            state.selected_cell.set(false);
            state.selected_th_or_cell.set(true);
            state.selected_row_unpinned.set(true);
        } else {
            state.selected_cell.set(true);
            state.selected_th_or_cell.set(true);
            state.selected_row_unpinned.set(!classes.contains(CLASS_PIN_ROW));
        }
        state.selected_col_unpinned.set(!classes.contains(CLASS_PIN_COL));
    }
}

pub fn create_editor(initial_data: Vec<serde_json::Value>) -> Result<El, String> {
    // Preprocess data
    let mut initial_add_columns = BTreeSet::new();
    for (i, row) in initial_data.iter().enumerate() {
        let serde_json::Value::Object(row) = row else {
            return Err(format!("Row {} is not an object", i));
        };
        for (k, _) in row {
            initial_add_columns.insert(k.clone());
        }
    }
    let mut initial_add_rows = vec![];
    for json_row in initial_data {
        let serde_json::Value::Object(mut json_row) = json_row else {
            panic!();
        };
        let mut row = vec![];
        for c in &initial_add_columns {
            let type_;
            let string;
            if let Some(cell) = json_row.remove(c) {
                match cell {
                    serde_json::Value::Null => {
                        type_ = ValueType::Null;
                        string = "".to_string();
                    },
                    serde_json::Value::Bool(v) => {
                        type_ = ValueType::Bool;
                        if v {
                            string = STR_TRUE.to_string();
                        } else {
                            string = STR_FALSE.to_string();
                        }
                    },
                    serde_json::Value::Number(v) => {
                        type_ = ValueType::Number;
                        string = v.to_string();
                    },
                    serde_json::Value::String(v) => {
                        type_ = ValueType::String;
                        string = v.clone();
                    },
                    serde_json::Value::Array(v) => {
                        type_ = ValueType::Json;
                        string = serde_json::to_string(&serde_json::Value::Array(v)).unwrap();
                    },
                    serde_json::Value::Object(v) => {
                        type_ = ValueType::Json;
                        string = serde_json::to_string(&serde_json::Value::Object(v)).unwrap();
                    },
                }
            } else {
                type_ = ValueType::Missing;
                string = "".to_string();
            }
            row.push(Value {
                type_: type_,
                string: string,
            });
        }
        initial_add_rows.push(row);
    }

    // Construct
    let root = el("div").classes(&[CLASS_ROOT]);

    // Create base editor framework
    let table_thr = el("tr");
    let table = el("table").push(el("tbody").push(table_thr.clone()));
    let state = Rc::new(State {
        root: root.weak(),
        table_thr: table_thr.weak(),
        undo: RefCell::new(vec![]),
        redo: RefCell::new(vec![]),
        last_change: Cell::new(Instant::now()),
        current_undo_level: RefCell::new(ChangeLevel {
            selection: None,
            changes: vec![],
        }),
        selected: RefCell::new(None),
        pin_resize_observer: OnceCell::new(),
        always: ObsBool::new(true),
        selected_th_or_cell: ObsBool::new(false),
        selected_cell: ObsBool::new(false),
        selected_col_unpinned: ObsBool::new(false),
        selected_row_unpinned: ObsBool::new(false),
        has_undo: ObsBool::new(false),
        has_redo: ObsBool::new(false),
    });
    state.pin_resize_observer.get_or_init(|| {
        return ResizeObserver::new({
            let state = state.clone();
            move |elements| {
                let mut min_x: Option<(usize, Element)> = None;
                let mut min_y: Option<(usize, Element)> = None;
                for el in elements {
                    let el = el.dyn_into::<Element>().unwrap();
                    let el_classes = el.class_list();
                    if el_classes.contains(CLASS_PIN_COL) {
                        let x = index_in_parent(&el);
                        if min_x.is_none() || min_x.as_ref().unwrap().0 > x {
                            min_x = Some((x, el.clone()));
                        }
                    }
                    if el_classes.contains(CLASS_PIN_ROW) {
                        let y = index_in_parent(&el.parent_element().unwrap());
                        if min_y.is_none() || min_y.as_ref().unwrap().0 > y {
                            min_y = Some((y, el.clone()));
                        }
                    }
                }
                if let Some((_, el)) = min_x {
                    update_column_pin_from(&state, &el);
                }
                if let Some((_, el)) = min_y {
                    update_row_pin_from(&el);
                }
            }
        });
    });
    let toolbar = el("div");
    let toolbar_tab_button_sheet = el_toolbar_tab_button("Sheet", {
        let state = state.clone();
        let toolbar = toolbar.clone();
        move |e| {
            build_toolbar_sheet(&state, &e, &toolbar);
        }
    });
    root.ref_extend(vec![
        //. .
        el("div").extend(vec![
            //. .
            toolbar_tab_button_sheet.clone(),
            el_toolbar_tab_button("Column", {
                let state = state.clone();
                let toolbar = toolbar.clone();
                move |e| {
                    build_toolbar_column(&state, &e, &toolbar);
                }
            }),
            el_toolbar_tab_button("Row", {
                let state = state.clone();
                let toolbar = toolbar.clone();
                move |e| {
                    build_toolbar_row(&state, &e, &toolbar);
                }
            }),
            el_toolbar_tab_button("Cell", {
                let state = state.clone();
                let toolbar = toolbar.clone();
                move |e| {
                    build_toolbar_cell(&state, &e, &toolbar);
                }
            })
        ]),
        toolbar.clone(),
        table.clone()
    ]);
    build_toolbar_sheet(&state, &toolbar_tab_button_sheet, &toolbar);

    // Add initial data
    apply(&state, Change::Splice(ChangeSplice {
        start: (X(0), Y(0)),
        remove: (X(0), Y(0)),
        add_columns: initial_add_columns.into_iter().map(|c| vec![v_str(c)]).collect(),
        add_rows: initial_add_rows,
    }));
    select(&state, (X(0), Y(0)));

    // Action event handling
    let text_obs;
    {
        let state = state.clone();
        let js_cb = Closure::wrap(Box::new(move |changes: Array, _| -> () {
            for change in changes {
                let change = change.dyn_into::<MutationRecord>().unwrap();
                let Some(cell) = find_containing_cell(&change.target().unwrap().dyn_into::<Node>().unwrap()) else {
                    continue;
                };
                let xy = get_xy(&cell);
                let cell_change;
                cell_change = Value::from_str(&cell.get_attribute(ATTR_TH_CELL_OLD_VALUE).unwrap()).unwrap();
                let new_value = get_value(&cell);
                cell.set_attribute(ATTR_TH_CELL_OLD_VALUE, &new_value.to_string()).unwrap();
                validate_cell(&cell, &new_value);
                push_undo(
                    &state,
                    Some(xy),
                    Change::Cells(ChangeCells { cells: [(xy, cell_change)].into_iter().collect() }),
                );
            }
        }) as Box<dyn Fn(Array, JsValue)>);
        text_obs = MutationObserver::new(js_cb.as_ref().unchecked_ref()).unwrap();
        root.ref_own(|_| js_cb);
    };
    text_obs.observe_with_options(&table.raw(), &{
        let o = MutationObserverInit::new();
        o.set_character_data(true);
        o.set_subtree(true);
        o
    }).unwrap();
    table.ref_on_with_options("keydown", EventListenerOptions {
        phase: EventListenerPhase::Bubble,
        passive: false,
    }, {
        let state = state.clone();
        move |ev| {
            let ev = ev.dyn_ref::<KeyboardEvent>().unwrap();
            if let Some(cell) = find_containing_cell(&ev.target().unwrap().dyn_into::<Node>().unwrap()) {
                let xy = get_xy(&cell);
                if cell.class_list().contains(CLASS_EDITING) {
                    match ev.key().as_str() {
                        "Escape" => {
                            stop_editing_cell(&cell);
                            ev.prevent_default();
                        },
                        _ => {
                            // nop
                        },
                    }
                } else {
                    match ev.key().as_str() {
                        "ArrowUp" => {
                            if ev.ctrl_key() && xy.1 >= Y(1) {
                                act_add_up(&state);
                                ev.prevent_default();
                            } else {
                                if xy.1 == Y(0) {
                                    return;
                                }
                                select(&state, (xy.0, xy.1 - Y(1)));
                                ev.prevent_default();
                            }
                        },
                        "ArrowDown" => {
                            if ev.ctrl_key() && xy.1 >= Y(1) {
                                act_add_down(&state);
                                ev.prevent_default();
                            } else {
                                if xy.1 == row_count(&state) {
                                    return;
                                }
                                select(&state, (xy.0, xy.1 + Y(1)));
                                ev.prevent_default();
                            }
                        },
                        "ArrowLeft" => {
                            if ev.ctrl_key() {
                                act_add_left(&state);
                                ev.prevent_default();
                            } else {
                                if xy.0 == X(0) {
                                    return;
                                }
                                select(&state, (xy.0 - X(1), xy.1));
                                ev.prevent_default();
                            }
                        },
                        "ArrowRight" => {
                            if ev.ctrl_key() {
                                act_add_right(&state);
                                ev.prevent_default();
                            } else {
                                if xy.0 == column_count(&state) {
                                    return;
                                }
                                select(&state, (xy.0 + X(1), xy.1));
                                ev.prevent_default();
                            }
                        },
                        "Enter" => {
                            start_editing_cell(&state, &cell);
                            ev.prevent_default();
                        },
                        _ => { },
                    }
                }
            }
        }
    });
    table.ref_on_with_options("click", EventListenerOptions {
        phase: EventListenerPhase::Bubble,
        passive: false,
    }, {
        let state = state.clone();
        move |ev| {
            let Some(cell) = find_containing_cell(&ev.target().unwrap().dyn_into::<Node>().unwrap()) else {
                return;
            };
            superif!({
                let Some(active_element) = document().active_element() else {
                    break 'unedit;
                };
                if active_element != cell {
                    break 'unedit;
                }
                let was_selected = superif!({
                    let selected = state.selected.borrow();
                    let Some(select_el) = selected.as_ref() else {
                        break 'no;
                    };
                    if &cell != select_el {
                        break 'no;
                    }
                    break true;
                } 'no {
                    break false;
                });
                if was_selected {
                    if !cell.class_list().contains(CLASS_EDITING) {
                        start_editing_cell(&state, &cell);
                        ev.prevent_default();
                    }
                } else {
                    register_select(&state, &cell);
                }
            } 'unedit {
                if cell.class_list().contains(CLASS_EDITING) {
                    stop_editing_cell(&cell);
                }
            });
        }
    });
    return Ok(root);
}

fn main() {
    console_error_panic_hook::set_once();
    set_root(
        vec![
            create_editor(
                vec![
                    serde_json::Value::Object(
                        [("".to_string(), serde_json::Value::String("".to_string()))].into_iter().collect(),
                    )
                ],
            ).unwrap()
        ],
    );
}
