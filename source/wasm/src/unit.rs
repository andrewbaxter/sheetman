use {
    derive_more::derive::{
        Add,
        Sub,
    },
    serde::{
        Deserialize,
        Serialize,
    },
};

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Add, Sub, Debug)]
pub(crate) struct X(pub(crate) i64);

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Add, Sub, Debug)]
pub(crate) struct Y(pub(crate) i64);

pub(crate) type XY = (X, Y);
