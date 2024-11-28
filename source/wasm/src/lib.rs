use {
    change::{
        flush_undo,
        push_undo,
        push_undo_no_merge,
        redo,
        undo,
        Change,
        ChangeCells,
        ChangeSplice,
        ChangeSpliceRowCol,
        ChangeState,
    },
    dom::{
        copy,
        ATTR_CONTENTEDITABLE,
        ATTR_CONTENTEDITABLE_PLAINTEXT,
        ATTR_TABINDEX,
        ATTR_TH_CELL_OLD_VALUE,
        CLASS_DISABLED,
        CLASS_EDITING,
        CLASS_FOCUSED,
        CLASS_HEAD,
        CLASS_MARKED,
        CLASS_PIN_COL,
        CLASS_PIN_ROW,
        CLASS_ROOT,
        CLASS_SUBMARKED,
        CLASS_TABLE_CONTAINER,
        CLASS_TOOLBAR,
        CLASS_TOOLBAR_TABS,
        CLASS_TOOLBAR_TAB_BUTTON_FRONT,
        ICON_ADD_DOWN,
        ICON_ADD_LEFT,
        ICON_ADD_RIGHT,
        ICON_ADD_UP,
        ICON_CLEAR,
        ICON_COPY,
        ICON_CUT,
        ICON_DELETE,
        ICON_FILL,
        ICON_MARK,
        ICON_PASTE,
        ICON_PIN,
        ICON_REDO,
        ICON_SHIFT_DOWN,
        ICON_SHIFT_UP,
        ICON_SORT,
        ICON_SORT_REV,
        ICON_TYPE_BOOL,
        ICON_TYPE_JSON,
        ICON_TYPE_MISSING,
        ICON_TYPE_NULL,
        ICON_TYPE_NUMBER,
        ICON_TYPE_STRING,
        ICON_UNDO,
        ICON_UNMARK,
        ICON_UNPIN,
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
    indexmap::{
        IndexMap,
        IndexSet,
    },
    js_sys::Array,
    obs::ObsBool,
    rooting::{
        el,
        El,
        ResizeObserver,
    },
    std::{
        cell::{
            OnceCell,
            RefCell,
        },
        cmp::Reverse,
        collections::{
            BTreeSet,
            HashMap,
        },
        rc::Rc,
        str::FromStr,
    },
    unit::{
        X,
        XY,
        Y,
    },
    value::{
        apply_cell_value,
        get_value,
        to_json,
        v_str,
        validate_cell,
        ExportValue,
        Value,
        ValueType,
        STR_FALSE,
        STR_TRUE,
    },
    wasm_bindgen::{
        closure::Closure,
        JsCast,
        JsValue,
    },
    wasm_bindgen_futures::spawn_local,
    web_sys::{
        Element,
        FocusOptions,
        HtmlElement,
        KeyboardEvent,
        MutationObserver,
        MutationObserverInit,
        MutationRecord,
        Node,
        ResizeObserverBoxOptions,
        ResizeObserverOptions,
    },
};

pub mod reexport {
    pub use indexmap;
}
pub(crate) mod obs;
pub mod dom;
pub mod value;
pub(crate) mod change;
pub(crate) mod unit;

struct State {
    root: El,
    tbody: El,
    always: ObsBool,
    focused: RefCell<Option<Element>>,
    focused_th_or_cell: ObsBool,
    focused_cell: ObsBool,
    focused_col_unpinned: ObsBool,
    focused_row_unpinned: ObsBool,
    focused_cell_not_marked: ObsBool,
    focused_cell_not_str: ObsBool,
    focused_cell_not_number: ObsBool,
    focused_cell_not_bool: ObsBool,
    focused_cell_not_json: ObsBool,
    focused_cell_not_null: ObsBool,
    focused_cell_not_missing: ObsBool,
    pin_resize_observer: OnceCell<ResizeObserver>,
    change: ChangeState,
    pinned_cols: RefCell<BTreeSet<X>>,
    pinned_rows: RefCell<BTreeSet<Y>>,
    mark: RefCell<Option<XY>>,
    hotkeys: HashMap<Hotkey, Rc<RefCell<Box<dyn FnMut(Editor)>>>>,
}

fn update_cell_pin(state: &Rc<State>, cell: &Element, col: bool, row: bool) {
    let classes = cell.class_list();
    classes.toggle_with_force(CLASS_PIN_COL, col).unwrap();
    classes.toggle_with_force(CLASS_PIN_ROW, row).unwrap();
    if col || row {
        let obs_opts = ResizeObserverOptions::new();
        obs_opts.set_box(ResizeObserverBoxOptions::BorderBox);
        state.pin_resize_observer.get().unwrap().0.js_resize_observer.observe_with_options(&cell, &obs_opts);
    } else {
        state.pin_resize_observer.get().unwrap().0.js_resize_observer.unobserve(&cell);
    }
}

fn maybe_pin_cell(state: &Rc<State>, xy: XY, pinned_cols: &BTreeSet<X>, pinned_rows: &BTreeSet<Y>, cell: &Element) {
    let pinned_col = pinned_cols.contains(&xy.0);
    let pinned_row = pinned_rows.contains(&xy.1);
    if pinned_col || pinned_row {
        update_cell_pin(state, cell, pinned_col, pinned_row);
    }
}

fn update_column_pin_offsets(state: &Rc<State>) {
    let rows = state.tbody.raw().children();

    #[derive(Clone)]
    struct PinCol {
        width: f64,
        cells: Vec<HtmlElement>,
    }

    let mut pin_cols = vec![];
    let pinned = state.pinned_cols.borrow();
    pin_cols.resize(pinned.len(), PinCol {
        width: 0.,
        cells: vec![],
    });
    let mut start_left = 0.;
    let grid_border_thickness = 1.;
    for rel_y in 0 .. rows.length() {
        let row = rows.item(rel_y).unwrap().children();
        for (x, pin_col) in Iterator::zip(pinned.iter(), pin_cols.iter_mut()) {
            let cell = row.item(x.0 as u32).unwrap().dyn_into::<HtmlElement>().unwrap();
            if rel_y == 0 {
                pin_col.width = cell.get_bounding_client_rect().width();
                if *x > X(0) {
                    start_left += grid_border_thickness + pin_col.width;
                }
            }
            pin_col.cells.push(cell);
        }
    }
    let mut prev_right = 0.;
    let mut prev_left = start_left;
    for (rel_x, pin_col) in pin_cols.into_iter().enumerate() {
        if rel_x > 0 {
            prev_left -= grid_border_thickness + pin_col.width;
        }
        for cell in &pin_col.cells {
            cell.style().set_property("left", &format!("{}px", prev_right)).unwrap();
            cell.style().set_property("right", &format!("{}px", prev_left)).unwrap();
        }
        prev_right += grid_border_thickness + pin_col.width;
    }
}

fn update_row_pin_offsets(state: &Rc<State>) {
    let rows = state.tbody.raw().children();

    #[derive(Clone)]
    struct PinRow {
        height: f64,
        cells: Vec<HtmlElement>,
    }

    let mut pin_rows = vec![];
    let pinned = state.pinned_rows.borrow();
    pin_rows.resize(pinned.len() + 1, PinRow {
        height: 0.,
        cells: vec![],
    });
    let mut sum_height = 0.;
    let grid_border_thickness = 1.;
    for (y, pin_row) in Iterator::zip(Iterator::chain(
        // Include header row - header row should never be pinned due to guards
        [Y(0)].iter(),
        // Plus other rows
        pinned.iter(),
    ), pin_rows.iter_mut()) {
        let row = rows.item(y.0 as u32).unwrap().children();
        for rel_x in 0 .. row.length() {
            let cell = row.item(rel_x).unwrap().dyn_into::<HtmlElement>().unwrap();
            if rel_x == 0 {
                pin_row.height = cell.get_bounding_client_rect().height();
                if *y > Y(0) {
                    sum_height += grid_border_thickness;
                    sum_height += pin_row.height;
                }
            }
            pin_row.cells.push(cell);
        }
    }
    let mut prev_bottom = 0.;
    let mut prev_top = sum_height;
    for (rel_y, pin_row) in pin_rows.into_iter().enumerate() {
        if rel_y > 0 {
            prev_top -= grid_border_thickness + pin_row.height;
        }
        for cell in &pin_row.cells {
            cell.style().set_property("top", &format!("{}px", prev_bottom)).unwrap();
            cell.style().set_property("bottom", &format!("{}px", prev_top)).unwrap();
        }
        prev_bottom += grid_border_thickness + pin_row.height;
    }
}

fn build_th(
    state: &Rc<State>,
    xy: XY,
    pinned_cols: &BTreeSet<X>,
    pinned_rows: &BTreeSet<Y>,
    title: &Value,
) -> Element {
    let out = document().create_element("th").unwrap();
    out.set_attribute(ATTR_TABINDEX, "0").unwrap();
    apply_cell_value(&out, xy.1, title);
    maybe_pin_cell(state, xy, pinned_cols, pinned_rows, &out);
    return out;
}

fn build_cell(
    state: &Rc<State>,
    xy: XY,
    pinned_cols: &BTreeSet<X>,
    pinned_rows: &BTreeSet<Y>,
    new_value: &Value,
) -> Element {
    let out = document().create_element("td").unwrap();
    out.set_attribute(ATTR_TABINDEX, "0").unwrap();
    apply_cell_value(&out, xy.1, new_value);
    maybe_pin_cell(state, xy, pinned_cols, pinned_rows, &out);
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

fn apply(state: &Rc<State>, change: Change) -> (Option<XY>, Change) {
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
                    apply_cell_value(&cell, xy.1, &new_value);
                } else {
                    out.insert(xy, get_value(&cell));
                    apply_cell_value(&cell, xy.1, &new_value);
                }
            }
            return (None, Change::Cells(ChangeCells { cells: out }));
        },
        Change::Splice(change) => {
            set_mark(state, None, None);
            let rows_parent = state.tbody.raw();
            let rows = rows_parent.children();
            let rev_change_remove = (X(change.add_columns.len() as i64), Y(change.add_rows.len() as i64));
            let old_selection = find_selection(state).map(|s| s.0);
            let initial_column_count = table_width(state);
            let initial_row_count = Y(rows.length() as i64);

            // Remove rows
            let mut pinned_rows = state.pinned_rows.borrow_mut();
            let mut rev_add_rows = vec![];
            for rel_y in 0 .. change.remove.1.0 {
                let y = change.start.1 + Y(rel_y);
                let row = rows.item(change.start.1.0 as u32).unwrap();
                let row_children = row.children();

                // Add removed row to reverse change
                let mut rev_add_row = vec![];
                for rel_x in 0 .. row_children.length() {
                    rev_add_row.push(get_value(&row_children.item(rel_x).unwrap()));
                }
                rev_add_rows.push(ChangeSpliceRowCol {
                    pinned: pinned_rows.contains(&y),
                    cells: rev_add_row,
                });

                // Remove it
                row.remove();
            }
            {
                // Shift row pins by row remove + add
                let adjust_y = Y(0) - change.remove.1 + Y(change.add_rows.len() as i64);
                for k in pinned_rows.split_off(&change.start.1) {
                    if k < change.start.1 + change.remove.1 {
                        continue;
                    }
                    pinned_rows.insert(k + adjust_y);
                }
            }

            // # Prep remove columns
            let mut pinned_cols = state.pinned_cols.borrow_mut();
            let mut rev_add_columns = vec![];
            for rel_x in 0 .. change.remove.0.0 {
                let x = change.start.0 + X(rel_x);
                rev_add_columns.push(ChangeSpliceRowCol {
                    pinned: pinned_cols.contains(&x),
                    cells: vec![],
                });
            }
            {
                // Shift column pins by col remove + add
                let adjust_x = X(0) - change.remove.0 + X(change.add_columns.len() as i64);
                for k in pinned_cols.split_off(&change.start.0) {
                    if k < change.start.0 + change.remove.0 {
                        continue;
                    }
                    pinned_cols.insert(k + adjust_x);
                }
            }

            // Register new pinned cols
            for (j, splice_col) in change.add_columns.iter().enumerate() {
                let x = change.start.0 + X(j as i64);
                if splice_col.pinned {
                    pinned_cols.insert(x);
                }
            }

            // Add/remove columns
            let create_rows_start_y;
            if rows.length() == 0 {
                create_rows_start_y = Y(1);

                // Add columns if no rows
                let new_row = document().create_element("tr").unwrap();
                for (j, col_values) in change.add_columns.iter().enumerate() {
                    let x = change.start.0 + X(j as i64);
                    if col_values.cells.len() != 1 {
                        panic!("See splice struct constraints in comments");
                    }
                    new_row
                        .append_child(
                            &build_th(
                                state,
                                (x, Y(0)),
                                &pinned_cols,
                                &pinned_rows,
                                &col_values.cells.get(0).unwrap(),
                            ),
                        )
                        .unwrap();
                }
                rows_parent.append_child(&new_row).unwrap();
            } else {
                create_rows_start_y = change.start.1;

                // Add/remove columns to each row
                for rel_y in 0 .. rows.length() {
                    let y = Y(rel_y as i64);
                    let row = rows.item(rel_y).unwrap();
                    let cells = row.children();

                    // Remove column cells
                    for rel_x in 0 .. change.remove.0.0 {
                        let cell = cells.item(change.start.0.0 as u32).unwrap();

                        // Add cells to reverse change
                        rev_add_columns.get_mut(rel_x as usize).unwrap().cells.push(get_value(&cell));

                        // Remove
                        cell.remove();
                    }

                    // Add new column cells
                    let add_cells_before = cells.item(change.start.0.0 as u32).map(|v| v.dyn_into::<Node>().unwrap());
                    for (j, splice_col) in change.add_columns.iter().enumerate() {
                        let x = change.start.0 + X(j as i64);
                        let cell_value = splice_col.cells.get(j as usize).unwrap();
                        let cell;
                        if rel_y == 0 {
                            cell = build_th(state, (x, y), &pinned_cols, &pinned_rows, &cell_value);
                        } else {
                            cell = build_cell(state, (x, y), &pinned_cols, &pinned_rows, &cell_value);
                        }
                        row
                            .insert_before(
                                &cell,
                                add_cells_before.clone().map(|v| v.dyn_into::<Node>().unwrap()).as_ref(),
                            )
                            .unwrap();
                    }
                }
            }

            // Add rows to each column
            let add_rows_before = rows.item(create_rows_start_y.0 as u32).map(|r| r.dyn_into::<Node>().unwrap());
            for (i, splice_row) in change.add_rows.into_iter().enumerate() {
                let y = create_rows_start_y + Y(i as i64);
                if splice_row.pinned {
                    pinned_rows.insert(y);
                }
                let new_row = document().create_element("tr").unwrap();
                for (j, cell_value) in splice_row.cells.into_iter().enumerate() {
                    new_row
                        .append_child(&build_cell(state, (X(j as i64), y), &pinned_cols, &pinned_rows, &cell_value))
                        .unwrap();
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

fn select(state: &Rc<State>, new_sel: XY) {
    let cell = get_cell(state, new_sel).unwrap().dyn_into::<HtmlElement>().unwrap();
    cell.focus().unwrap();
    {
        let mark = *state.mark.borrow();
        sync_mark(state, Some(new_sel), mark);
    }
    register_select(state, &cell);
}

fn table_width(state: &State) -> X {
    let Some(r) = state.tbody.raw().first_element_child() else {
        return X(0);
    };
    return X(r.children().length() as i64);
}

fn table_height(state: &State) -> Y {
    return Y(state.tbody.raw().children().length() as i64);
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

fn get_cell(state: &State, coord2: XY) -> Option<Element> {
    let Some(row) = state.tbody.raw().children().item((coord2.1.0) as u32) else {
        return None;
    };
    return row.children().item((coord2.0.0) as u32);
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
            push_undo(&state.change, Some(xy), rev_change);
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
    let old_tab_buttons = state.root.raw().get_elements_by_class_name(CLASS_TOOLBAR_TAB_BUTTON_FRONT);
    for i in 0 .. old_tab_buttons.length() {
        old_tab_buttons.item(i).unwrap().class_list().remove_1(CLASS_TOOLBAR_TAB_BUTTON_FRONT).unwrap();
    }
    button.ref_classes(&[CLASS_TOOLBAR_TAB_BUTTON_FRONT]);
}

fn get_xy(cell: &Element) -> XY {
    fn index_in_parent(e: &Element) -> usize {
        let sibs = e.parent_element().unwrap().children();
        for i in 0 .. sibs.length() {
            if &sibs.item(i).unwrap() == e {
                return i as usize;
            }
        }
        panic!();
    }

    return (X(index_in_parent(&cell) as i64), Y(index_in_parent(&cell.parent_element().unwrap()) as i64));
}

fn find_selection(state: &Rc<State>) -> Option<(XY, Element)> {
    let focused = state.tbody.raw().get_elements_by_class_name(CLASS_FOCUSED);
    let Some(cell) = focused.item(0) else {
        return None;
    };
    return Some((get_xy(&cell), cell));
}

struct FindMarkedRes {
    sel_xy: XY,
    sel: Element,
    marked: Vec<Vec<(XY, Element)>>,
}

fn find_marked(state: &Rc<State>) -> Option<FindMarkedRes> {
    let Some((sel_xy, sel)) = find_selection(state) else {
        return None;
    };
    if let Some(mark) = *state.mark.borrow() {
        let rows = state.tbody.raw().children();
        let mut out_rows = vec![];
        for y in sel_xy.1.min(mark.1).0 ..= sel_xy.1.max(mark.1).0 {
            let y = Y(y);
            let row = rows.item(y.0 as u32).unwrap().children();
            let mut out_row = vec![];
            for x in sel_xy.0.min(mark.0).0 ..= sel_xy.0.max(mark.0).0 {
                let x = X(x);
                out_row.push(((x, y), row.item(x.0 as u32).unwrap()));
            }
            out_rows.push(out_row);
        }
        return Some(FindMarkedRes {
            sel_xy: sel_xy,
            sel: sel,
            marked: out_rows,
        });
    } else {
        return Some(FindMarkedRes {
            sel_xy: sel_xy,
            sel: sel.clone(),
            marked: vec![vec![(sel_xy, sel)]],
        });
    }
}

fn act_undo(state: &Rc<State>, current_sel: Option<XY>) {
    undo(&state.change, current_sel, |c| apply(&state, c), |s| select(&state, s));
}

fn act_redo(state: &Rc<State>, current_sel: Option<XY>) {
    redo(&state.change, current_sel, |c| apply(&state, c), |s| select(&state, s));
}

fn act_add_left(state: &Rc<State>) {
    let Some((sel_xy, _)) = find_selection(state) else {
        return;
    };
    let change = Change::Splice(ChangeSplice {
        start: (sel_xy.0, Y(0)),
        remove: (X(0), Y(0)),
        add_columns: vec![ChangeSpliceRowCol {
            pinned: false,
            cells: (0 .. table_height(&state).0).map(|_| v_str("")).collect(),
        }],
        add_rows: vec![],
    });
    flush_undo(&state.change);
    push_undo(&state.change, Some(sel_xy), apply(&state, change).1);
    flush_undo(&state.change);
    select(&state, sel_xy);
}

fn act_add_right(state: &Rc<State>) {
    let Some((sel_xy, _)) = find_selection(state) else {
        return;
    };
    let change = Change::Splice(ChangeSplice {
        start: (sel_xy.0 + X(1), Y(0)),
        remove: (X(0), Y(0)),
        add_columns: vec![ChangeSpliceRowCol {
            pinned: false,
            cells: (0 .. table_height(&state).0).map(|_| v_str("")).collect(),
        }],
        add_rows: vec![],
    });
    flush_undo(&state.change);
    push_undo(&state.change, Some(sel_xy), apply(&state, change).1);
    flush_undo(&state.change);
    select(&state, (sel_xy.0 + X(1), sel_xy.1));
}

fn act_add_up(state: &Rc<State>) {
    let Some((sel_xy, _)) = find_selection(state) else {
        return;
    };
    if sel_xy.1 <= Y(1) {
        return;
    }
    let change = Change::Splice(ChangeSplice {
        start: (X(0), sel_xy.1),
        remove: (X(0), Y(0)),
        add_columns: vec![],
        add_rows: vec![ChangeSpliceRowCol {
            pinned: false,
            cells: copy_row_values(state, sel_xy.1),
        }],
    });
    flush_undo(&state.change);
    push_undo(&state.change, Some(sel_xy), apply(&state, change).1);
    flush_undo(&state.change);
    select(&state, sel_xy);
}

fn act_add_down(state: &Rc<State>) {
    let Some((sel_xy, _)) = find_selection(state) else {
        return;
    };
    if sel_xy.1 == Y(0) {
        return;
    }
    let change = Change::Splice(ChangeSplice {
        start: (X(0), sel_xy.1 + Y(1)),
        remove: (X(0), Y(0)),
        add_columns: vec![],
        add_rows: vec![ChangeSpliceRowCol {
            pinned: false,
            cells: copy_row_values(state, sel_xy.1),
        }],
    });
    flush_undo(&state.change);
    push_undo(&state.change, Some(sel_xy), apply(&state, change).1);
    flush_undo(&state.change);
    select(&state, (sel_xy.0, sel_xy.1 + Y(1)));
}

fn act_sort(state: &Rc<State>, rev: bool) {
    let Some((sel_xy, _)) = find_selection(&state) else {
        return;
    };
    let mut value_rows = vec![];
    let rows = state.tbody.raw().children();
    for rel_y in 1 .. rows.length() {
        let row = rows.item(rel_y).unwrap().children();
        let mut value_row = vec![];
        for rel_x in 0 .. row.length() {
            let el_cell = row.item(rel_x).unwrap();
            value_row.push(get_value(&el_cell));
        }
        value_rows.push(value_row);
    }
    let col_idx = sel_xy.0.0;
    if rev {
        value_rows.sort_by_cached_key(|r| Reverse(r.get(col_idx as usize).unwrap().clone()));
    } else {
        value_rows.sort_by_cached_key(|r| r.get(col_idx as usize).unwrap().clone());
    }
    let mut change = ChangeCells { cells: HashMap::new() };
    for (row_idx, row) in value_rows.into_iter().enumerate() {
        for (col_idx, cell) in row.into_iter().enumerate() {
            change.cells.insert((X(row_idx as i64 + 1), Y(col_idx as i64)), cell);
        }
    }
    flush_undo(&state.change);
    push_undo(&state.change, Some(sel_xy), apply(&state, Change::Cells(change)).1);
    flush_undo(&state.change);
}

fn copy_row_values(state: &Rc<State>, y: Y) -> Vec<Value> {
    let base_row = state.tbody.raw().children().item(y.0 as u32).unwrap().children();
    let mut cells = vec![];
    for rel_x in 0 .. base_row.length() {
        let base_cell = base_row.item(rel_x).unwrap();
        cells.push(Value {
            type_: get_value(&base_cell).type_,
            string: "".to_string(),
        });
    }
    return cells;
}

fn build_toolbar_sheet(state: &Rc<State>, button: &El, toolbar: &El, sheet_actions: &[EditorAction]) {
    replace_toolbar_tab_front(&state, &button);
    toolbar.ref_clear();
    for action in sheet_actions {
        let cb = action.cb.clone();
        toolbar.ref_push(el_toolbar_button(&action.text, &action.icon, &state.always, {
            let state = state.clone();
            move |_| {
                (*cb.as_ref().borrow_mut())(Editor { state: state.clone() });
            }
        }));
    }
    toolbar.ref_extend(vec![
        //. .
        el_toolbar_button("Clear", ICON_CLEAR, &state.always, {
            let state = state.clone();
            move |_| {
                let (new_sel, rev_change) = apply(&state, Change::Splice(ChangeSplice {
                    start: (X(0), Y(0)),
                    remove: (table_width(&state), table_height(&state)),
                    add_columns: vec![ChangeSpliceRowCol {
                        pinned: false,
                        cells: vec![v_str("")],
                    }],
                    add_rows: vec![ChangeSpliceRowCol {
                        pinned: false,
                        cells: vec![v_str("")],
                    }],
                }));
                flush_undo(&state.change);
                push_undo_no_merge(&state.change, find_selection(&state).map(|s| s.0), rev_change);
                flush_undo(&state.change);
                if let Some(new_sel) = new_sel {
                    select(&state, new_sel);
                }
            }
        }),
        el_toolbar_button("Undo", ICON_UNDO, &state.change.has_undo, {
            let state = state.clone();
            move |_| act_undo(&state, find_selection(&state).map(|s| s.0))
        }),
        el_toolbar_button("Redo", ICON_REDO, &state.change.has_redo, {
            let state = state.clone();
            move |_| act_redo(&state, find_selection(&state).map(|s| s.0))
        })
    ]);
}

fn build_toolbar_column(state: &Rc<State>, button: &El, toolbar: &El) {
    replace_toolbar_tab_front(&state, &button);
    toolbar.ref_clear();
    toolbar.ref_extend(vec![
        //. .
        el_toolbar_button_2state([("Pin", ICON_PIN), ("Unpin", ICON_UNPIN)], &state.always, &state.focused_col_unpinned, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, _)) = find_selection(&state) else {
                    return;
                };
                let want_col_pinned = !state.pinned_cols.borrow().contains(&sel_xy.0);
                if want_col_pinned {
                    state.pinned_cols.borrow_mut().insert(sel_xy.0);
                } else {
                    state.pinned_cols.borrow_mut().remove(&sel_xy.0);
                }
                let rows = state.tbody.raw().children();
                for rel_y in 0 .. rows.length() {
                    let row_pinned = state.pinned_rows.borrow().contains(&Y(rel_y as i64));
                    update_cell_pin(
                        &state,
                        &rows.item(rel_y).unwrap().children().item(sel_xy.0.0 as u32).unwrap(),
                        want_col_pinned,
                        row_pinned,
                    );
                }
                update_column_pin_offsets(&state);
                state.focused_col_unpinned.set(!want_col_pinned);
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
                let col_count = table_width(&state);
                let change = Change::Splice(ChangeSplice {
                    start: (sel_xy.0, Y(0)),
                    remove: (X(1), Y(0)),
                    add_columns: if col_count > X(1) {
                        vec![]
                    } else {
                        vec![ChangeSpliceRowCol {
                            pinned: false,
                            cells: (0 .. table_height(&state).0).map(|_| v_str("")).collect(),
                        }]
                    },
                    add_rows: vec![],
                });
                let (new_sel, rev_change) = apply(&state, change);
                flush_undo(&state.change);
                push_undo(&state.change, Some(sel_xy), rev_change);
                flush_undo(&state.change);
                if let Some(new_sel) = new_sel {
                    select(&state, new_sel);
                }
            }
        }),
        el_toolbar_button("Sort", ICON_SORT, &state.always, {
            let state = state.clone();
            move |_| act_sort(&state, false)
        }),
        el_toolbar_button("Sort reversed", ICON_SORT_REV, &state.always, {
            let state = state.clone();
            move |_| act_sort(&state, true)
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
            &state.focused_cell,
            &state.focused_row_unpinned,
            {
                let state = state.clone();
                move |_| {
                    let Some((sel_xy, _)) = find_selection(&state) else {
                        return;
                    };
                    let want_row_pinned = !state.pinned_rows.borrow().contains(&sel_xy.1);
                    if want_row_pinned {
                        state.pinned_rows.borrow_mut().insert(sel_xy.1);
                    } else {
                        state.pinned_rows.borrow_mut().remove(&sel_xy.1);
                    }
                    let row = state.tbody.raw().children().item(sel_xy.1.0 as u32).unwrap().children();
                    for rel_x in 0 .. row.length() {
                        let col_pinned = state.pinned_cols.borrow().contains(&X(rel_x as i64));
                        update_cell_pin(&state, &row.item(rel_x).unwrap(), col_pinned, want_row_pinned);
                    }
                    update_row_pin_offsets(&state);
                    state.focused_row_unpinned.set(!want_row_pinned);
                }
            },
        ),
        el_toolbar_button("Add above", ICON_ADD_UP, &state.focused_cell, {
            let state = state.clone();
            move |_| act_add_up(&state)
        }),
        el_toolbar_button("Add below", ICON_ADD_DOWN, &state.focused_cell, {
            let state = state.clone();
            move |_| act_add_down(&state)
        }),
        el_toolbar_button("Delete", ICON_DELETE, &state.focused_cell, {
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
                flush_undo(&state.change);
                push_undo(&state.change, Some(sel_xy), rev_change);
                flush_undo(&state.change);
                if let Some(new_sel) = new_sel {
                    select(&state, new_sel);
                }
            }
        }),
        el_toolbar_button("Shift up", ICON_SHIFT_UP, &state.focused_cell, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, _)) = find_selection(&state) else {
                    return;
                };
                if sel_xy.1 <= Y(1) {
                    return;
                }
                flush_undo(&state.change);

                // Remove row
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
                push_undo_no_merge(&state.change, Some(sel_xy), rev_change.clone());

                // Add row
                let change = Change::Splice(ChangeSplice {
                    start: (X(0), sel_xy.1 - Y(1)),
                    remove: (X(0), Y(0)),
                    add_rows: rev_change_splice.add_rows.clone(),
                    add_columns: vec![],
                });
                let rev_change = apply(&state, change.clone()).1;
                push_undo_no_merge(&state.change, Some(sel_xy), rev_change.clone());
                flush_undo(&state.change);

                // Done
                select(&state, (sel_xy.0, sel_xy.1 - Y(1)));
            }
        }),
        el_toolbar_button("Shift down", ICON_SHIFT_DOWN, &state.focused_cell, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, _)) = find_selection(&state) else {
                    return;
                };
                if sel_xy.1 + Y(1) == table_height(&state) {
                    return;
                }
                flush_undo(&state.change);

                // Remove row
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
                push_undo_no_merge(&state.change, Some(sel_xy), rev_change.clone());

                // Add row
                let rev_change = apply(&state, Change::Splice(ChangeSplice {
                    start: (X(0), sel_xy.1 + Y(1)),
                    remove: (X(0), Y(0)),
                    add_rows: rev_change_splice.add_rows.clone(),
                    add_columns: vec![],
                })).1;
                push_undo_no_merge(&state.change, Some(sel_xy), rev_change.clone());
                flush_undo(&state.change);

                // Done
                select(&state, (sel_xy.0, sel_xy.1 + Y(1)));
            }
        })
    ]);
}

fn sync_mark(state: &Rc<State>, new_sel: Option<XY>, new_mark: Option<XY>) {
    let old_marked = state.tbody.raw().get_elements_by_class_name(CLASS_MARKED);
    for i in 0 .. old_marked.length() {
        old_marked.item(i).unwrap().class_list().remove_1(CLASS_MARKED).unwrap();
    }
    let old_submarked = state.tbody.raw().get_elements_by_class_name(CLASS_SUBMARKED);
    for i in 0 .. old_submarked.length() {
        old_submarked.item(i).unwrap().class_list().remove_1(CLASS_SUBMARKED).unwrap();
    }
    let rows = state.tbody.raw().children();
    match (new_sel, new_mark) {
        (Some(sel), Some(mark)) => {
            for y in sel.1.min(mark.1).0 ..= sel.1.max(mark.1).0 {
                let y = Y(y);
                let row = rows.item(y.0 as u32).unwrap().children();
                for x in sel.0.min(mark.0).0 ..= sel.0.max(mark.0).0 {
                    let x = X(x);
                    if (x, y) == mark {
                        row.item(x.0 as u32).unwrap().class_list().add_1(CLASS_MARKED).unwrap();
                    } else {
                        row.item(x.0 as u32).unwrap().class_list().add_1(CLASS_SUBMARKED).unwrap();
                    }
                }
            }
        },
        _ => { },
    }
}

fn set_mark(state: &Rc<State>, current_sel: Option<XY>, new_mark: Option<XY>) {
    sync_mark(state, current_sel, new_mark);
    *state.mark.borrow_mut() = new_mark;
    state.focused_cell_not_marked.set(new_mark.is_none());
}

fn act_mark(state: &Rc<State>) {
    let Some((sel_xy, cell)) = find_selection(&state) else {
        return;
    };
    set_mark(state, Some(sel_xy), if cell.class_list().contains(CLASS_MARKED) {
        None
    } else {
        Some(sel_xy)
    });
}

fn act_copy(state: &Rc<State>) {
    let Some(found) = find_marked(state) else {
        return;
    };
    let mut out_rows = vec![];
    for row in found.marked {
        let mut out_row = vec![];
        for (_, cell) in row {
            out_row.push(get_value(&cell))
        }
        out_rows.push(out_row);
    }
    copy(out_rows);
}

fn act_cut(state: &Rc<State>) {
    let Some(found) = find_marked(state) else {
        return;
    };
    let mut change_cells = HashMap::new();
    let mut out_rows = vec![];
    for row in found.marked {
        let mut out_row = vec![];
        for (xy, cell) in row {
            out_row.push(get_value(&cell));
            change_cells.insert(xy, Value {
                type_: ValueType::String,
                string: "".to_string(),
            });
        }
        out_rows.push(out_row);
    }
    copy(out_rows);
    flush_undo(&state.change);
    let (_, rev_change) = apply(state, Change::Cells(ChangeCells { cells: change_cells }));
    push_undo_no_merge(&state.change, Some(found.sel_xy), rev_change);
    flush_undo(&state.change);
}

fn act_paste(state: &Rc<State>) {
    let Some((sel_xy, _)) = find_selection(state) else {
        return;
    };
    let state = state.clone();
    spawn_local(async move {
        let text = wasm_bindgen_futures::JsFuture::from(window().navigator().clipboard().read_text()).await.unwrap();
        let data_rows = serde_json::from_str::<Vec<Vec<Value>>>(&text.as_string().unwrap()).unwrap();
        let mut change_cells = HashMap::new();
        let w = table_width(&state);
        let h = table_height(&state);
        for (i, row) in data_rows.into_iter().enumerate() {
            let y = Y(i as i64);
            if y >= h {
                break;
            }
            for (j, cell) in row.into_iter().enumerate() {
                let x = X(j as i64);
                if x >= w {
                    break;
                }
                change_cells.insert((x, y), cell);
            }
        }
        flush_undo(&state.change);
        let (_, rev_change) = apply(&state, Change::Cells(ChangeCells { cells: change_cells }));
        push_undo_no_merge(&state.change, Some(sel_xy), rev_change);
        flush_undo(&state.change);
    });
}

fn act_fill(state: &Rc<State>) {
    let Some(found) = find_marked(state) else {
        return;
    };
    if found.marked.is_empty() {
        return;
    }
    let mut change_cells = HashMap::new();

    struct InterpNumber {
        start_y: Y,
        end_y: Y,
        start: f64,
        end: f64,
        sig_digits: usize,
    }

    enum Interp {
        Number(InterpNumber),
        Other(Value),
    }

    let mut interp = vec![];
    for (
        (cell_start_xy, cell_start),
        (cell_end_xy, cell_end),
    ) in Iterator::zip(found.marked.first().unwrap().iter(), found.marked.last().unwrap().iter()) {
        let val_start = get_value(&cell_start);
        let val_end = get_value(&cell_end);
        let interp_col;
        shed!{
            'col _;
            shed!{
                if val_start.type_ != ValueType::Number {
                    break;
                }
                if val_start.type_ != val_end.type_ {
                    break;
                }
                let Ok(n_start) = val_start.string.parse::<f64>() else {
                    break;
                };
                let Ok(n_end) = val_end.string.parse::<f64>() else {
                    break;
                };
                let mut sig_digits = 0;
                if let Some((_, digits)) = val_start.string.split_once(".") {
                    sig_digits = digits.len();
                }
                interp_col = Interp::Number(InterpNumber {
                    start_y: cell_start_xy.1,
                    end_y: cell_end_xy.1,
                    start: n_start,
                    end: n_end,
                    sig_digits: sig_digits,
                });
                break 'col;
            }
            // Default case - just fill
            interp_col = Interp::Other(val_start.clone());
            change_cells.insert(*cell_end_xy, val_start);
        };
        interp.push(interp_col);
    }
    for row in found.marked.iter().skip(1) {
        for (interp_col, (xy, _)) in Iterator::zip(interp.iter(), row.iter()) {
            match interp_col {
                Interp::Number(i) => {
                    if xy.1 == i.end_y {
                        continue;
                    }
                    let t = (xy.1 - i.start_y).0 as f64 / (i.end_y - i.start_y).0 as f64;
                    change_cells.insert(*xy, Value {
                        type_: ValueType::Number,
                        string: format!("{:.1$}", i.start * t + i.end * (1. - t), i.sig_digits),
                    });
                },
                Interp::Other(i) => {
                    change_cells.insert(*xy, i.clone());
                },
            }
        }
    }
    flush_undo(&state.change);
    let (_, rev_change) = apply(state, Change::Cells(ChangeCells { cells: change_cells }));
    push_undo_no_merge(&state.change, Some(found.sel_xy), rev_change);
    flush_undo(&state.change);
}

fn build_toolbar_cell(state: &Rc<State>, button: &El, toolbar: &El) {
    replace_toolbar_tab_front(&state, &button);
    toolbar.ref_clear();
    toolbar.ref_extend(vec![
        //. .
        el_toolbar_button_2state(
            [("Mark", ICON_MARK), ("Unmark", ICON_UNMARK)],
            &state.focused_cell,
            &state.focused_cell_not_marked,
            {
                let state = state.clone();
                move |_| act_mark(&state)
            },
        ),
        el_toolbar_button("Copy", ICON_COPY, &state.focused_cell, {
            let state = state.clone();
            move |_| act_copy(&state)
        }),
        el_toolbar_button("Cut", ICON_CUT, &state.focused_cell, {
            let state = state.clone();
            move |_| act_cut(&state)
        }),
        el_toolbar_button("Paste", ICON_PASTE, &state.focused_cell, {
            let state = state.clone();
            move |_| act_paste(&state)
        }),
        el_toolbar_button("Fill", ICON_FILL, &state.focused_cell, {
            let state = state.clone();
            move |_| act_fill(&state)
        }),
        el_toolbar_button("String", ICON_TYPE_STRING, &state.focused_cell_not_str, {
            let state = state.clone();
            move |_| {
                let Some(found) = find_marked(&state) else {
                    return;
                };
                let mut change_cells = HashMap::new();
                for row in found.marked {
                    for (xy, cell) in row {
                        if xy.1 == Y(0) {
                            continue;
                        }
                        let value = get_value(&cell);
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
                        change_cells.insert(xy, Value {
                            type_: ValueType::String,
                            string: new_string,
                        });
                    }
                }
                let (new_sel, rev_change) = apply(&state, Change::Cells(ChangeCells { cells: change_cells }));
                if new_sel.is_some() {
                    panic!();
                }
                push_undo(&state.change, Some(found.sel_xy), rev_change);
                update_value_obsbools(&state, &found.sel);
            }
        }),
        el_toolbar_button("Number", ICON_TYPE_NUMBER, &state.focused_cell_not_number, {
            let state = state.clone();
            move |_| {
                let Some(found) = find_marked(&state) else {
                    return;
                };
                let mut change_cells = HashMap::new();
                for row in found.marked {
                    for (xy, cell) in row {
                        if xy.1 == Y(0) {
                            return;
                        }
                        let value = get_value(&cell);
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
                        change_cells.insert(xy, Value {
                            type_: ValueType::Number,
                            string: new_string,
                        });
                    }
                }
                let (new_sel, rev_change) = apply(&state, Change::Cells(ChangeCells { cells: change_cells }));
                if new_sel.is_some() {
                    panic!();
                }
                push_undo(&state.change, Some(found.sel_xy), rev_change);
                update_value_obsbools(&state, &found.sel);
            }
        }),
        el_toolbar_button("Bool", ICON_TYPE_BOOL, &state.focused_cell_not_bool, {
            let state = state.clone();
            move |_| {
                let Some(found) = find_marked(&state) else {
                    return;
                };
                let mut change_cells = HashMap::new();
                for row in found.marked {
                    for (xy, cell) in row {
                        if xy.1 == Y(0) {
                            return;
                        }
                        let value = get_value(&cell);
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
                        change_cells.insert(xy, Value {
                            type_: ValueType::Bool,
                            string: new_string,
                        });
                    }
                }
                let (new_sel, rev_change) = apply(&state, Change::Cells(ChangeCells { cells: change_cells }));
                if new_sel.is_some() {
                    panic!();
                }
                push_undo(&state.change, Some(found.sel_xy), rev_change);
                update_value_obsbools(&state, &found.sel);
            }
        }),
        el_toolbar_button("Json", ICON_TYPE_JSON, &state.focused_cell_not_json, {
            let state = state.clone();
            move |_| {
                let Some(found) = find_marked(&state) else {
                    return;
                };
                let mut change_cells = HashMap::new();
                for row in found.marked {
                    for (xy, cell) in row {
                        if xy.1 == Y(0) {
                            return;
                        }
                        let value = get_value(&cell);
                        let new_string;
                        if let Some(json) = to_json(&value) {
                            new_string = serde_json::to_string(&json).unwrap();
                        } else {
                            new_string = value.string;
                        };
                        change_cells.insert(xy, Value {
                            type_: ValueType::Json,
                            string: new_string,
                        });
                    }
                }
                let (new_sel, rev_change) = apply(&state, Change::Cells(ChangeCells { cells: change_cells }));
                if new_sel.is_some() {
                    panic!();
                }
                push_undo(&state.change, Some(found.sel_xy), rev_change);
                update_value_obsbools(&state, &found.sel);
            }
        }),
        el_toolbar_button("Null", ICON_TYPE_NULL, &state.focused_cell_not_null, {
            let state = state.clone();
            move |_| {
                let Some(found) = find_marked(&state) else {
                    return;
                };
                let mut change_cells = HashMap::new();
                for row in found.marked {
                    for (xy, _) in row {
                        if xy.1 == Y(0) {
                            continue;
                        }
                        change_cells.insert(xy, Value {
                            type_: ValueType::Null,
                            string: "".to_string(),
                        });
                    }
                }
                let (new_sel, rev_change) = apply(&state, Change::Cells(ChangeCells { cells: change_cells }));
                if new_sel.is_some() {
                    panic!();
                }
                push_undo(&state.change, Some(found.sel_xy), rev_change);
                update_value_obsbools(&state, &found.sel);
            }
        }),
        el_toolbar_button("Missing", ICON_TYPE_MISSING, &state.focused_cell_not_missing, {
            let state = state.clone();
            move |_| {
                let Some(found) = find_marked(&state) else {
                    return;
                };
                let mut change_cells = HashMap::new();
                for row in found.marked {
                    for (xy, _) in row {
                        if xy.1 == Y(0) {
                            continue;
                        }
                        change_cells.insert(xy, Value {
                            type_: ValueType::Missing,
                            string: "".to_string(),
                        });
                    }
                }
                let (new_sel, rev_change) = apply(&state, Change::Cells(ChangeCells { cells: change_cells }));
                if new_sel.is_some() {
                    panic!();
                }
                push_undo(&state.change, Some(found.sel_xy), rev_change);
                update_value_obsbools(&state, &found.sel);
            }
        })
    ]);
}

fn update_value_obsbools(state: &Rc<State>, cell: &Element) {
    let v = get_value(&cell);
    state.focused_cell_not_bool.set(v.type_ != ValueType::Bool);
    state.focused_cell_not_str.set(v.type_ != ValueType::String);
    state.focused_cell_not_number.set(v.type_ != ValueType::Number);
    state.focused_cell_not_json.set(v.type_ != ValueType::Json);
    state.focused_cell_not_null.set(v.type_ != ValueType::Null);
    state.focused_cell_not_missing.set(v.type_ != ValueType::Missing);
}

fn register_select(state: &Rc<State>, cell: &Element) {
    let classes = cell.class_list();
    if !classes.contains(CLASS_FOCUSED) {
        // Deselect old
        let old_focused = state.tbody.raw().get_elements_by_class_name(CLASS_FOCUSED);
        for i in 0 .. old_focused.length() {
            let cell = old_focused.item(i).unwrap();
            cell.class_list().remove_1(CLASS_FOCUSED).unwrap();
            stop_editing_cell(&cell);
        }

        // Select new
        classes.add_1(CLASS_FOCUSED).unwrap();
        let xy = get_xy(&cell);
        *state.focused.borrow_mut() = Some(cell.clone());
        if xy.1 == Y(0) {
            state.focused_cell.set(false);
            state.focused_th_or_cell.set(true);
            state.focused_row_unpinned.set(true);
            state.focused_cell_not_bool.set(false);
            state.focused_cell_not_str.set(false);
            state.focused_cell_not_number.set(false);
            state.focused_cell_not_json.set(false);
            state.focused_cell_not_null.set(false);
            state.focused_cell_not_missing.set(false);
        } else {
            state.focused_cell.set(true);
            state.focused_th_or_cell.set(true);
            state.focused_row_unpinned.set(!state.pinned_rows.borrow().contains(&xy.1));
            update_value_obsbools(state, cell);
        }
        state.focused_col_unpinned.set(!state.pinned_cols.borrow().contains(&xy.0));
        state.focused_cell_not_marked.set(*state.mark.borrow() != Some(xy));
    }
}

#[derive(Clone)]
pub struct Editor {
    state: Rc<State>,
}

impl Editor {
    pub fn root(&self) -> El {
        return self.state.root.clone();
    }

    pub fn set_data(&self, data: Vec<IndexMap<String, ExportValue>>) -> Result<(), String> {
        let value_data = export_to_splice(data)?;
        let (_, rev_change) = apply(&self.state, Change::Splice(ChangeSplice {
            start: (X(0), Y(0)),
            remove: (table_width(&self.state), table_height(&self.state)),
            add_columns: value_data.columns,
            add_rows: value_data.rows,
        }));
        flush_undo(&self.state.change);
        push_undo(&self.state.change, find_selection(&self.state).map(|s| s.0), rev_change);
        flush_undo(&self.state.change);
        select(&self.state, (X(0), Y(0)));
        return Ok(());
    }

    pub fn get_data(&self) -> Vec<IndexMap<String, ExportValue>> {
        let rows = self.state.tbody.raw().children();
        let mut columns = vec![];
        let mut out = vec![];
        for y in 0 .. rows.length() {
            let row = rows.item(y).unwrap().children();
            if y == 0 {
                for x in 0 .. row.length() {
                    let cell = row.item(x).unwrap();
                    let v = get_value(&cell);
                    columns.push(v.string);
                }
            } else {
                let mut row_out = IndexMap::new();
                for x in 0 .. row.length() {
                    let cell = row.item(x).unwrap();
                    let v = get_value(&cell);
                    match v.type_ {
                        ValueType::Missing => {
                            continue;
                        },
                        _ => { },
                    }
                    row_out.insert(columns.get(x as usize).cloned().unwrap(), ExportValue {
                        type_: v.type_,
                        string: v.string,
                    });
                }
                out.push(row_out);
            }
        }
        return out;
    }
}

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Hotkey {
    pub key: String,
    pub ctrl: bool,
    pub alt: bool,
}

#[derive(Clone)]
pub struct EditorAction {
    pub text: String,
    pub icon: String,
    pub hotkeys: Vec<Hotkey>,
    pub cb: Rc<RefCell<Box<dyn FnMut(Editor)>>>,
}

pub struct EditorOptions {
    pub sheet_actions: Vec<EditorAction>,
    pub initial_data: Vec<IndexMap<String, ExportValue>>,
}

// Columns and rows for splice that clears everything first (1-cell columns, then
// with correct cell counts)
struct InitialSpliceColsRows {
    columns: Vec<ChangeSpliceRowCol>,
    rows: Vec<ChangeSpliceRowCol>,
}

fn export_to_splice(source: Vec<IndexMap<String, ExportValue>>) -> Result<InitialSpliceColsRows, String> {
    let mut add_columns = IndexSet::new();
    for row in &source {
        for (k, _) in row {
            add_columns.insert(k.clone());
        }
    }
    let mut add_rows = vec![];
    for json_row in source {
        let mut row = vec![];
        for c in &add_columns {
            let type_;
            let string;
            if let Some(cell) = json_row.get(c) {
                type_ = cell.type_;
                string = cell.string.clone();
            } else {
                type_ = ValueType::Missing;
                string = "".to_string();
            }
            row.push(Value {
                type_: type_,
                string: string,
            });
        }
        add_rows.push(ChangeSpliceRowCol {
            pinned: false,
            cells: row,
        });
    }
    if add_rows.is_empty() {
        return Err("Initial data is empty!".to_string());
    }
    return Ok(InitialSpliceColsRows {
        columns: add_columns.into_iter().map(|c| ChangeSpliceRowCol {
            pinned: false,
            cells: vec![v_str(c)],
        }).collect(),
        rows: add_rows,
    });
}

pub fn create_editor(opts: EditorOptions) -> Result<Editor, String> {
    // Preprocess data
    let initial = export_to_splice(opts.initial_data)?;

    // Construct
    let root = el("div").classes(&[CLASS_ROOT]);

    // Create base editor framework
    let tbody = el("tbody");
    let table = el("table").push(tbody.clone());
    let state = Rc::new(State {
        root: root.clone(),
        tbody: tbody,
        change: ChangeState::new(),
        focused: RefCell::new(None),
        pin_resize_observer: OnceCell::new(),
        always: ObsBool::new(true),
        focused_th_or_cell: ObsBool::new(false),
        focused_cell: ObsBool::new(false),
        focused_col_unpinned: ObsBool::new(false),
        focused_row_unpinned: ObsBool::new(false),
        focused_cell_not_marked: ObsBool::new(true),
        focused_cell_not_str: ObsBool::new(false),
        focused_cell_not_number: ObsBool::new(false),
        focused_cell_not_bool: ObsBool::new(false),
        focused_cell_not_json: ObsBool::new(false),
        focused_cell_not_null: ObsBool::new(false),
        focused_cell_not_missing: ObsBool::new(false),
        pinned_cols: Default::default(),
        pinned_rows: Default::default(),
        mark: RefCell::new(None),
        hotkeys: opts
            .sheet_actions
            .iter()
            .flat_map(|a| a.hotkeys.iter().map(|k| (k.clone(), a.cb.clone())))
            .collect(),
    });
    state.pin_resize_observer.get_or_init(|| {
        return ResizeObserver::new({
            let state = state.clone();
            move |_| {
                if !state.pinned_cols.borrow().is_empty() {
                    update_column_pin_offsets(&state);
                }
                if !state.pinned_rows.borrow().is_empty() {
                    update_row_pin_offsets(&state);
                }
            }
        });
    });
    let toolbar = el("div").classes(&[CLASS_TOOLBAR]);
    let toolbar_tab_button_sheet = el_toolbar_tab_button("Sheet", {
        let state = state.clone();
        let toolbar = toolbar.clone();
        let sheet_actions = opts.sheet_actions.clone();
        move |e| {
            build_toolbar_sheet(&state, &e, &toolbar, &sheet_actions);
        }
    });
    let top = el("div").classes(&[CLASS_HEAD]);
    top.ref_push(el("div").classes(&[CLASS_TOOLBAR_TABS]).extend(vec![
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
    ]));
    top.ref_push(toolbar.clone());
    let table_container = el("div").classes(&[CLASS_TABLE_CONTAINER]).push(table.clone());
    root.ref_extend(vec![top, table_container.clone()]);
    build_toolbar_sheet(&state, &toolbar_tab_button_sheet, &toolbar, &opts.sheet_actions);

    // Add initial data
    apply(&state, Change::Splice(ChangeSplice {
        start: (X(0), Y(0)),
        remove: (X(0), Y(0)),
        add_columns: initial.columns,
        add_rows: initial.rows,
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
                let old_value = Value::from_str(&cell.get_attribute(ATTR_TH_CELL_OLD_VALUE).unwrap()).unwrap();
                let new_value = get_value(&cell);
                cell.set_attribute(ATTR_TH_CELL_OLD_VALUE, &new_value.to_string()).unwrap();
                push_undo(
                    &state.change,
                    Some(xy),
                    Change::Cells(ChangeCells { cells: [(xy, old_value)].into_iter().collect() }),
                );
                validate_cell(&cell, xy.1, &new_value);
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
    table_container.ref_on("mouseover", {
        let state = state.clone();
        move |_| {
            let Some((_, e)) = find_selection(&state) else {
                return;
            };
            e.dyn_into::<HtmlElement>().unwrap().focus_with_options(&{
                let o = FocusOptions::new();
                o.set_prevent_scroll(true);
                o
            }).unwrap();
        }
    }).ref_on_with_options("keydown", EventListenerOptions {
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
                        "Enter" => {
                            if !ev.shift_key() {
                                stop_editing_cell(&cell);
                                ev.prevent_default();
                            }
                        },
                        _ => {
                            // nop
                        },
                    }
                } else {
                    for (hotkey, cb) in &state.hotkeys {
                        if hotkey.key == ev.key() && hotkey.ctrl == ev.ctrl_key() && hotkey.alt == ev.alt_key() {
                            (*cb.borrow_mut())(Editor { state: state.clone() });
                            ev.prevent_default();
                            return;
                        }
                    }
                    match ev.key().as_str() {
                        "ArrowUp" | "k" => {
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
                        "ArrowDown" | "j" => {
                            if ev.ctrl_key() && xy.1 >= Y(1) {
                                act_add_down(&state);
                                ev.prevent_default();
                            } else {
                                if xy.1 + Y(1) == table_height(&state) {
                                    return;
                                }
                                select(&state, (xy.0, xy.1 + Y(1)));
                                ev.prevent_default();
                            }
                        },
                        "ArrowLeft" | "h" => {
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
                        "ArrowRight" | "l" => {
                            if ev.ctrl_key() {
                                act_add_right(&state);
                                ev.prevent_default();
                            } else {
                                if xy.0 + X(1) == table_width(&state) {
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
                        "m" => {
                            act_mark(&state);
                            ev.prevent_default();
                        },
                        "x" => {
                            if ev.ctrl_key() {
                                act_cut(&state);
                                ev.prevent_default();
                            }
                        },
                        "c" => {
                            if ev.ctrl_key() {
                                act_copy(&state);
                                ev.prevent_default();
                            }
                        },
                        "v" => {
                            if ev.ctrl_key() {
                                act_paste(&state);
                                ev.prevent_default();
                            }
                        },
                        "u" => {
                            if ev.shift_key() {
                                act_redo(&state, Some(xy));
                            } else {
                                act_undo(&state, Some(xy));
                            }
                            ev.prevent_default();
                        },
                        "z" => {
                            if ev.ctrl_key() {
                                act_undo(&state, Some(xy));
                                ev.prevent_default();
                            }
                        },
                        "y" => {
                            if ev.ctrl_key() {
                                act_redo(&state, Some(xy));
                                ev.prevent_default();
                            }
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
                let was_focused = superif!({
                    let focused = state.focused.borrow();
                    let Some(select_el) = focused.as_ref() else {
                        break 'no;
                    };
                    if &cell != select_el {
                        break 'no;
                    }
                    break true;
                } 'no {
                    break false;
                });
                if was_focused {
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
    return Ok(Editor { state: state });
}
