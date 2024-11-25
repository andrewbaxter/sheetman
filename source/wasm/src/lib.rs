use {
    change::{
        act_redo,
        act_undo,
        flush_undo,
        push_undo,
        push_undo_no_merge,
        Change,
        ChangeCells,
        ChangeSplice,
        ChangeSpliceRowCol,
        ChangeState,
    },
    dom::{
        ATTR_CONTENTEDITABLE,
        ATTR_CONTENTEDITABLE_PLAINTEXT,
        ATTR_TABINDEX,
        ATTR_TH_CELL_OLD_VALUE,
        CLASS_DISABLED,
        CLASS_EDITING,
        CLASS_HEAD,
        CLASS_PIN_COL,
        CLASS_PIN_ROW,
        CLASS_ROOT,
        CLASS_SELECTED,
        CLASS_TABLE_CONTAINER,
        CLASS_TOOLBAR,
        CLASS_TOOLBAR_TABS,
        CLASS_TOOLBAR_TAB_BUTTON_FRONT,
        ICON_ADD_DOWN,
        ICON_ADD_LEFT,
        ICON_ADD_RIGHT,
        ICON_ADD_UP,
        ICON_CLEAR,
        ICON_DELETE,
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
        Coord2,
        X,
        Y,
    },
    value::{
        apply_cell_value,
        get_value,
        to_json,
        v_str,
        validate_cell,
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

pub(crate) mod obs;
pub mod dom;
pub(crate) mod value;
pub(crate) mod change;
pub(crate) mod unit;

struct State {
    root: El,
    tbody: El,
    always: ObsBool,
    selected: RefCell<Option<Element>>,
    selected_th_or_cell: ObsBool,
    selected_cell: ObsBool,
    selected_col_unpinned: ObsBool,
    selected_row_unpinned: ObsBool,
    selected_cell_not_str: ObsBool,
    selected_cell_not_number: ObsBool,
    selected_cell_not_bool: ObsBool,
    selected_cell_not_json: ObsBool,
    selected_cell_not_null: ObsBool,
    selected_cell_not_missing: ObsBool,
    pin_resize_observer: OnceCell<ResizeObserver>,
    change: ChangeState,
    pinned_cols: RefCell<BTreeSet<X>>,
    pinned_rows: RefCell<BTreeSet<Y>>,
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

fn maybe_pin_cell(
    state: &Rc<State>,
    xy: Coord2,
    pinned_cols: &BTreeSet<X>,
    pinned_rows: &BTreeSet<Y>,
    cell: &Element,
) {
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
    xy: Coord2,
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
    xy: Coord2,
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
                    apply_cell_value(&cell, xy.1, &new_value);
                } else {
                    out.insert(xy, get_value(&cell));
                    apply_cell_value(&cell, xy.1, &new_value);
                }
            }
            return (None, Change::Cells(ChangeCells { cells: out }));
        },
        Change::Splice(change) => {
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
            let add_rows_before = rows.item(change.start.1.0 as u32 + 1).map(|r| r.dyn_into::<Node>().unwrap());
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

fn select(state: &Rc<State>, new_sel: Coord2) {
    let cell = get_cell(state, new_sel).unwrap().dyn_into::<HtmlElement>().unwrap();
    cell.focus().unwrap();
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

fn get_cell(state: &State, coord2: Coord2) -> Option<Element> {
    let Some(row) = state.tbody.raw().children().item((coord2.1.0) as u32) else {
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

fn get_xy(cell: &Element) -> Coord2 {
    return (X(index_in_parent(&cell) as i64), Y(index_in_parent(&cell.parent_element().unwrap()) as i64));
}

fn find_selection(state: &Rc<State>) -> Option<(Coord2, Element)> {
    let selected = state.tbody.raw().get_elements_by_class_name(CLASS_SELECTED);
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
                push_undo(&state.change, find_selection(&state).map(|s| s.0), rev_change);
                flush_undo(&state.change);
                if let Some(new_sel) = new_sel {
                    select(&state, new_sel);
                }
            }
        }),
        el_toolbar_button("Undo", ICON_UNDO, &state.change.has_undo, {
            let state = state.clone();
            move |_| act_undo(
                &state.change,
                find_selection(&state).map(|s| s.0),
                |c| apply(&state, c),
                |s| select(&state, s),
            )
        }),
        el_toolbar_button("Redo", ICON_REDO, &state.change.has_redo, {
            let state = state.clone();
            move |_| act_redo(
                &state.change,
                find_selection(&state).map(|s| s.0),
                |c| apply(&state, c),
                |s| select(&state, s),
            )
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
                state.selected_col_unpinned.set(!want_col_pinned);
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
            &state.selected_cell,
            &state.selected_row_unpinned,
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
                    state.selected_row_unpinned.set(!want_row_pinned);
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
                flush_undo(&state.change);
                push_undo(&state.change, Some(sel_xy), rev_change);
                flush_undo(&state.change);
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
                flush_undo(&state.change);
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
                let rev_change = Change::Splice(ChangeSplice {
                    start: (X(0), sel_xy.1 - Y(1)),
                    remove: (X(0), Y(0)),
                    add_rows: rev_change_splice.add_rows.clone(),
                    add_columns: vec![],
                });
                push_undo_no_merge(&state.change, Some(sel_xy), rev_change.clone());
                flush_undo(&state.change);
                select(&state, (sel_xy.0, sel_xy.1 - Y(1)));
            }
        }),
        el_toolbar_button("Shift down", ICON_SHIFT_DOWN, &state.selected_cell, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, _)) = find_selection(&state) else {
                    return;
                };
                if sel_xy.1 + Y(1) == table_height(&state) {
                    return;
                }
                flush_undo(&state.change);
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
                let rev_change = Change::Splice(ChangeSplice {
                    start: (X(0), sel_xy.1 + Y(1)),
                    remove: (X(0), Y(0)),
                    add_rows: rev_change_splice.add_rows.clone(),
                    add_columns: vec![],
                });
                push_undo_no_merge(&state.change, Some(sel_xy), rev_change.clone());
                flush_undo(&state.change);
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
        el_toolbar_button("String", ICON_TYPE_STRING, &state.selected_cell_not_str, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, cell)) = find_selection(&state) else {
                    return;
                };
                if sel_xy.1 == Y(0) {
                    return;
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
                let cell_change = (sel_xy, Value {
                    type_: ValueType::String,
                    string: new_string,
                });
                let (new_sel, rev_change) =
                    apply(&state, Change::Cells(ChangeCells { cells: [cell_change].into_iter().collect() }));
                if new_sel.is_some() {
                    panic!();
                }
                push_undo(&state.change, Some(sel_xy), rev_change);
                update_value_obsbools(&state, &cell);
            }
        }),
        el_toolbar_button("Number", ICON_TYPE_NUMBER, &state.selected_cell_not_number, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, cell)) = find_selection(&state) else {
                    return;
                };
                if sel_xy.1 == Y(0) {
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
                let cell_change = (sel_xy, Value {
                    type_: ValueType::Number,
                    string: new_string,
                });
                let (new_sel, rev_change) =
                    apply(&state, Change::Cells(ChangeCells { cells: [cell_change].into_iter().collect() }));
                if new_sel.is_some() {
                    panic!();
                }
                push_undo(&state.change, Some(sel_xy), rev_change);
                update_value_obsbools(&state, &cell);
            }
        }),
        el_toolbar_button("Bool", ICON_TYPE_BOOL, &state.selected_cell_not_bool, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, cell)) = find_selection(&state) else {
                    return;
                };
                if sel_xy.1 == Y(0) {
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
                let cell_change = (sel_xy, Value {
                    type_: ValueType::Bool,
                    string: new_string,
                });
                let (new_sel, rev_change) =
                    apply(&state, Change::Cells(ChangeCells { cells: [cell_change].into_iter().collect() }));
                if new_sel.is_some() {
                    panic!();
                }
                push_undo(&state.change, Some(sel_xy), rev_change);
                update_value_obsbools(&state, &cell);
            }
        }),
        el_toolbar_button("Json", ICON_TYPE_JSON, &state.selected_cell_not_json, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, cell)) = find_selection(&state) else {
                    return;
                };
                if sel_xy.1 == Y(0) {
                    return;
                }
                let value = get_value(&cell);
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
                push_undo(&state.change, Some(sel_xy), rev_change);
                update_value_obsbools(&state, &cell);
            }
        }),
        el_toolbar_button("Null", ICON_TYPE_NULL, &state.selected_cell_not_null, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, cell)) = find_selection(&state) else {
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
                push_undo(&state.change, Some(sel_xy), rev_change);
                update_value_obsbools(&state, &cell);
            }
        }),
        el_toolbar_button("Missing", ICON_TYPE_MISSING, &state.selected_cell_not_missing, {
            let state = state.clone();
            move |_| {
                let Some((sel_xy, cell)) = find_selection(&state) else {
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
                push_undo(&state.change, Some(sel_xy), rev_change);
                update_value_obsbools(&state, &cell);
            }
        })
    ]);
}

fn update_value_obsbools(state: &Rc<State>, cell: &Element) {
    let v = get_value(&cell);
    state.selected_cell_not_bool.set(v.type_ != ValueType::Bool);
    state.selected_cell_not_str.set(v.type_ != ValueType::String);
    state.selected_cell_not_number.set(v.type_ != ValueType::Number);
    state.selected_cell_not_json.set(v.type_ != ValueType::Json);
    state.selected_cell_not_null.set(v.type_ != ValueType::Null);
    state.selected_cell_not_missing.set(v.type_ != ValueType::Missing);
}

fn register_select(state: &Rc<State>, cell: &Element) {
    let classes = cell.class_list();
    if !classes.contains(CLASS_SELECTED) {
        // Deselect old
        let old_selected = state.tbody.raw().get_elements_by_class_name(CLASS_SELECTED);
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
            state.selected_cell_not_bool.set(false);
            state.selected_cell_not_str.set(false);
            state.selected_cell_not_number.set(false);
            state.selected_cell_not_json.set(false);
            state.selected_cell_not_null.set(false);
            state.selected_cell_not_missing.set(false);
        } else {
            state.selected_cell.set(true);
            state.selected_th_or_cell.set(true);
            state.selected_row_unpinned.set(!state.pinned_rows.borrow().contains(&xy.1));
            update_value_obsbools(state, cell);
        }
        state.selected_col_unpinned.set(!state.pinned_cols.borrow().contains(&xy.0));
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

    pub fn set_data(&self, data: Vec<serde_json::Value>) -> Result<(), String> {
        let value_data = json_to_splice(data)?;
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

    pub fn get_data(&self) -> Vec<serde_json::Value> {
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
                let mut row_out = serde_json::Map::new();
                for x in 0 .. row.length() {
                    let cell = row.item(x).unwrap();
                    let v = get_value(&cell);
                    match v.type_ {
                        ValueType::Missing => {
                            continue;
                        },
                        _ => { },
                    }
                    row_out.insert(
                        columns.get(x as usize).cloned().unwrap(),
                        to_json(&v).unwrap_or_else(|| serde_json::Value::String(v.string)),
                    );
                }
                out.push(serde_json::Value::Object(row_out));
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
    pub initial_data: Vec<serde_json::Value>,
}

// Columns and rows for splice that clears everything first (1-cell columns, then
// with correct cell counts)
struct InitialSpliceColsRows {
    columns: Vec<ChangeSpliceRowCol>,
    rows: Vec<ChangeSpliceRowCol>,
}

fn json_to_splice(source: Vec<serde_json::Value>) -> Result<InitialSpliceColsRows, String> {
    let mut add_columns = BTreeSet::new();
    for (i, row) in source.iter().enumerate() {
        let serde_json::Value::Object(row) = row else {
            return Err(format!("Row {} is not an object", i));
        };
        for (k, _) in row {
            add_columns.insert(k.clone());
        }
    }
    let mut add_rows = vec![];
    for json_row in source {
        let serde_json::Value::Object(mut json_row) = json_row else {
            panic!();
        };
        let mut row = vec![];
        for c in &add_columns {
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
    let initial = json_to_splice(opts.initial_data)?;

    // Construct
    let root = el("div").classes(&[CLASS_ROOT]);

    // Create base editor framework
    let tbody = el("tbody");
    let table = el("table").push(tbody.clone());
    let state = Rc::new(State {
        root: root.clone(),
        tbody: tbody,
        change: ChangeState::new(),
        selected: RefCell::new(None),
        pin_resize_observer: OnceCell::new(),
        always: ObsBool::new(true),
        selected_th_or_cell: ObsBool::new(false),
        selected_cell: ObsBool::new(false),
        selected_col_unpinned: ObsBool::new(false),
        selected_row_unpinned: ObsBool::new(false),
        selected_cell_not_str: ObsBool::new(false),
        selected_cell_not_number: ObsBool::new(false),
        selected_cell_not_bool: ObsBool::new(false),
        selected_cell_not_json: ObsBool::new(false),
        selected_cell_not_null: ObsBool::new(false),
        selected_cell_not_missing: ObsBool::new(false),
        pinned_cols: Default::default(),
        pinned_rows: Default::default(),
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
                                if xy.1 + Y(1) == table_height(&state) {
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
    return Ok(Editor { state: state });
}
