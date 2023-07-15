use std::collections::HashMap;

use crate::AstNodes;
use AstNodes::*;

pub fn var_map()->HashMap<String, Value>{
    let map = HashMap::from([
        ("nice".to_string(),Value::Num(69.0)),
    ]);
    map
}
