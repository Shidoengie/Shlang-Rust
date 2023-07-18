use std::collections::HashMap;

use crate::ast_nodes;
use ast_nodes::*;

pub fn var_map()->HashMap<String, Value>{
    let map = HashMap::from([
        ("nice".to_string(),Value::Num(69.0)),
    ]);
    map
}
