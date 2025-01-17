use {
    crate::{
        obs::ObsBool,
        unit::XY,
        value::Value,
    },
    flowcontrol::shed,
    std::{
        cell::{
            Cell,
            RefCell,
        },
        collections::HashMap,
    },
    web_time::{
        Duration,
        Instant,
    },
};

pub(crate) struct ChangeState {
    undo: RefCell<Vec<ChangeLevel>>,
    redo: RefCell<Vec<ChangeLevel>>,
    last_change: Cell<Instant>,
    current_undo_level: RefCell<ChangeLevel>,
    pub(crate) has_undo: ObsBool,
    pub(crate) has_redo: ObsBool,
}

impl ChangeState {
    pub(crate) fn new() -> Self {
        return ChangeState {
            undo: Default::default(),
            redo: Default::default(),
            last_change: Cell::new(Instant::now()),
            current_undo_level: RefCell::new(ChangeLevel {
                selection: None,
                changes: vec![],
            }),
            has_redo: ObsBool::new(false),
            has_undo: ObsBool::new(false),
        };
    }
}

#[derive(Clone)]
pub(crate) struct ChangeCells {
    pub(crate) cells: HashMap<XY, Value>,
}

#[derive(Clone)]
pub(crate) struct ChangeSpliceRowCol {
    pub(crate) pinned: bool,
    pub(crate) cells: Vec<Value>,
}

#[derive(Clone)]
pub(crate) struct ChangeSplice {
    pub(crate) start: XY,
    pub(crate) remove: XY,
    /// Each element is a column, with entries for all rows not including added rows.
    /// i.e. This can't have more elements than there are rows remaining after removal
    /// (+1 for heading).
    ///
    /// There's one exception: in an empty table, a set of columns with 1 value each
    /// (header) is allowed. This is done automatically once at startup and only them.
    pub(crate) add_columns: Vec<ChangeSpliceRowCol>,
    /// Each element is a row, with entries for all columns including added columns
    pub(crate) add_rows: Vec<ChangeSpliceRowCol>,
}

#[derive(Clone)]
pub(crate) enum Change {
    Cells(ChangeCells),
    Splice(ChangeSplice),
}

#[derive(Clone)]
pub(crate) struct ChangeLevel {
    selection: Option<XY>,
    changes: Vec<Change>,
}

pub(crate) fn flush_undo(state: &ChangeState) {
    let mut current_undo_level = state.current_undo_level.borrow_mut();
    if current_undo_level.changes.is_empty() {
        return;
    }
    state.undo.borrow_mut().push(current_undo_level.clone());
    current_undo_level.changes.clear();
    state.has_undo.set(true);
}

pub(crate) fn push_undo_no_merge(state: &ChangeState, sel: Option<XY>, change: Change) {
    let mut current_undo_level = state.current_undo_level.borrow_mut();
    if current_undo_level.changes.is_empty() {
        current_undo_level.selection = sel;
    }
    current_undo_level.changes.push(change);
    state.has_undo.set(true);
    state.redo.borrow_mut().clear();
    state.has_redo.set(false);
}

pub(crate) fn push_undo(state: &ChangeState, sel: Option<XY>, change: Change) {
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

pub(crate) fn undo(
    state: &ChangeState,
    current_selection: Option<XY>,
    mut apply: impl FnMut(Change) -> (Option<XY>, Change),
    select: impl FnOnce(XY),
) {
    flush_undo(state);
    let level = state.undo.borrow_mut().pop();
    if let Some(mut level) = level {
        let mut rev_level = ChangeLevel {
            selection: current_selection,
            changes: vec![],
        };
        let mut last_apply_sel = None;
        while let Some(change) = level.changes.pop() {
            let (apply_sel, rev_change) = apply(change);
            rev_level.changes.push(rev_change);
            last_apply_sel = apply_sel;
        }
        state.redo.borrow_mut().push(rev_level);
        state.has_redo.set(true);
        if state.undo.borrow().is_empty() {
            state.has_undo.set(false);
        }
        if let Some(sel) = level.selection.or(last_apply_sel) {
            select(sel);
        }
    }
}

pub(crate) fn redo(
    state: &ChangeState,
    current_selection: Option<XY>,
    mut apply: impl FnMut(Change) -> (Option<XY>, Change),
    select: impl FnOnce(XY),
) {
    flush_undo(state);
    let level = state.redo.borrow_mut().pop();
    if let Some(mut level) = level {
        let mut rev_level = ChangeLevel {
            selection: current_selection,
            changes: vec![],
        };
        let mut last_apply_sel = None;
        while let Some(change) = level.changes.pop() {
            let (apply_sel, rev_change) = apply(change);
            rev_level.changes.push(rev_change);
            last_apply_sel = apply_sel;
        }
        state.undo.borrow_mut().push(rev_level);
        state.has_undo.set(true);
        if let Some(sel) = level.selection.or(last_apply_sel) {
            select(sel);
        }
    }
}
