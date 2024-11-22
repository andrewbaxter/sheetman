use {
    rooting::{
        scope_any,
        ScopeValue,
    },
    std::{
        cell::RefCell,
        mem::swap,
        rc::{
            Rc,
            Weak,
        },
    },
};

struct ObsBool_ {
    value: bool,
    cbs: Vec<Weak<RefCell<Box<dyn FnMut(bool)>>>>,
}

#[derive(Clone)]
pub(crate) struct ObsBool(Rc<RefCell<ObsBool_>>);

impl ObsBool {
    pub(crate) fn new(initial: bool) -> Self {
        return Self(Rc::new(RefCell::new(ObsBool_ {
            value: initial,
            cbs: vec![],
        })));
    }

    pub(crate) fn get(&self) -> bool {
        return self.0.as_ref().borrow().value;
    }

    pub(crate) fn set(&self, v: bool) {
        let mut s = self.0.as_ref().borrow_mut();
        if v == s.value {
            return;
        }
        s.value = v;
        let mut cbs = vec![];
        swap(&mut cbs, &mut s.cbs);
        for cb in cbs {
            let Some(cb) = cb.upgrade() else {
                continue;
            };
            (*cb.as_ref().borrow_mut())(v);
            s.cbs.push(Rc::downgrade(&cb));
        }
    }

    pub(crate) fn listen(&self, mut cb: impl FnMut(bool) + 'static) -> ScopeValue {
        cb(self.0.as_ref().borrow().value);
        let cb = Rc::new(RefCell::new(Box::new(cb) as Box<dyn FnMut(bool)>));
        self.0.as_ref().borrow_mut().cbs.push(Rc::downgrade(&cb));
        return scope_any(cb);
    }
}
