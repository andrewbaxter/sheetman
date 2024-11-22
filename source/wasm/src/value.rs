use {
    crate::dom::{
        ATTR_TH_CELL_OLD_VALUE,
        ATTR_VALUE_TYPE,
        ATTR_VALUE_TYPE_BOOL,
        ATTR_VALUE_TYPE_JSON,
        ATTR_VALUE_TYPE_MISSING,
        ATTR_VALUE_TYPE_NULL,
        ATTR_VALUE_TYPE_NUMBER,
        ATTR_VALUE_TYPE_STR,
        CLASS_INVALID,
    },
    serde::{
        Deserialize,
        Serialize,
    },
    std::str::FromStr,
    wasm_bindgen::JsValue,
    web_sys::{
        console,
        Element,
    },
};

pub(crate) static STR_TRUE: &str = "true";
pub(crate) static STR_FALSE: &str = "false";

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename = "snake_case")]
pub(crate) enum ValueType {
    String,
    Bool,
    Number,
    Json,
    Null,
    Missing,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename = "snake_case")]
pub(crate) struct Value {
    pub(crate) type_: ValueType,
    pub(crate) string: String,
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

pub(crate) fn get_value(e: &Element) -> Value {
    // Conteteditable keeps random un-deletable newlines, better of two bad choices
    let text = e.text_content().unwrap().trim().to_string();
    let Some(type_) = e.get_attribute(ATTR_VALUE_TYPE) else {
        console::log_2(&JsValue::from(&format!("Cell missing attribute {}", ATTR_VALUE_TYPE)), &JsValue::from(e));
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
        console::log_2(&JsValue::from("Cell unknown value type attribute"), &JsValue::from(e));
        return v_str(text);
    }
    return Value {
        type_: out_type,
        string: text,
    };
}

pub(crate) fn to_json(v: &Value) -> Option<serde_json::Value> {
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
                        serde_json::Number::from_f64(n).unwrap_or(serde_json::Number::from_f64(0.).unwrap()),
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

pub(crate) fn v_str(v: impl AsRef<str>) -> Value {
    return Value {
        type_: ValueType::String,
        string: v.as_ref().to_string(),
    };
}

pub(crate) fn apply_cell_value(cell: &Element, new_value: &Value) {
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

pub(crate) fn validate_cell(cell: &Element, v: &Value) {
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
