use crate::frontend::nodes::*;
use crate::Interpreter;
use crate::Parser;
use slotmap::SlotMap;

use std::collections::HashMap;

use std::f64::consts::{E, PI, TAU};
use std::fs;

use functions::*;
use std::io;
use std::io::Write;
use std::thread;
use std::time;
use std::time::Duration;
use std::time::SystemTime;

use super::scope::Scope;
const NULL: Value = Value::Null;
pub fn default_scope() -> Scope {
    Scope {
        parent: None,
        vars: var_map(),
        structs: HashMap::from([]),
    }
}

macro_rules! vars {
    () => {
        HashMap::new()
    };

    ($($t:tt)*) => {
        vars_internal!([] $($t)*)
    };
}

macro_rules! vars_internal {
    ([$($acc:tt)*]) => {
        HashMap::from([$($acc)*])
    };

    ([$($acc:tt)*] $key:ident => $val:expr $(, $($left:tt)*)? ) => {
        vars_internal!([$($acc)* (stringify!($key).to_string(),$val),] $($($left)*)? )
    };

    ([$($acc:tt)*] $name:ident($func:ident,$size:expr) $(, $($left:tt)*)? ) => {
        vars_internal!([$($acc)* (
            stringify!($name).to_string(),
            Value::BuiltinFunc(BuiltinFunc::new(
                stringify!($name).to_string(),
                $func,$size
            ))
        ),] $($($left)*)? )
    };
}
pub fn var_map() -> VarMap {
    vars![
        noice => Value::Num(69.0),
        PI => Value::Num(PI),
        TAU => Value::Num(TAU),
        wait(wait_builtin,1),
        time(unix_time,0),
        open_file(open_textfile,1),
        input(input_builtin,-1),
        println(println_builtin,-1),
        print(print_builtin,-1),
        parse_num(parse_num,1),
        to_str(to_str,1),
        stringify(stringify_vals,-1),
        typeof(typeof_node,1),
        eval(eval,1),
        max(max,2),
        min(min,2),
        sqrt(sqrt,1),
        sin(sin,1),
        cos(cos,1),
        tan(tan,1),
        pow(pow,2),
        import(import_var,1),
        del(delete_var,1),
        range(range,-1),
        floor(floor,1),
        round(round,1)
    ]
}

pub fn num_struct() -> Struct {
    let env = vars![
        to_str(to_str, 1),
        max(max, 2),
        min(min, 2),
        sqrt(sqrt, 1),
        sin(sin, 1),
        cos(cos, 1),
        tan(tan, 1),
        pow(pow, 2),
        floor(floor, 1),
        round(round, 1)
    ];

    Struct {
        id: None,
        env: Scope::from_vars(env),
    }
}
pub fn list_struct() -> Struct {
    use list_methods::*;
    let env = vars![
        len(list_len, 1),
        push(list_push, 2),
        pop(list_pop, 1),
        remove(list_remove, 2),
        pop_at(list_pop_at, 2)
    ];
    Struct {
        id: None,
        env: Scope::from_vars(env),
    }
}
pub(super) mod list_methods {
    use super::*;
    pub fn list_pop(_: &mut Scope, args: Vec<Value>, heap: &mut SlotMap<RefKey, Value>) -> Value {
        let Value::Ref(key) = &args[0] else {
            return NULL;
        };
        let Value::List(mut list) = heap.get(*key).unwrap().clone() else {
            return NULL;
        };
        let Some(value) = list.pop() else {
            return NULL;
        };
        heap[*key] = Value::List(list);
        return value;
    }
    pub fn list_remove(
        _: &mut Scope,
        args: Vec<Value>,
        heap: &mut SlotMap<RefKey, Value>,
    ) -> Value {
        let [Value::Ref(key), target] = &args[..2] else {
            return NULL;
        };
        let Value::List(list) = heap.get(*key).unwrap().clone() else {
            return NULL;
        };
        let old_len = list.len();
        let new_list: Vec<Value> = list.into_iter().filter(|v| v != target).collect();
        let new_len = new_list.len();
        heap[*key] = Value::List(new_list);
        Value::Num((old_len - new_len) as f64)
    }
    pub fn list_len(_: &mut Scope, args: Vec<Value>, heap: &mut SlotMap<RefKey, Value>) -> Value {
        let Value::Ref(key) = &args[0] else {
            return NULL;
        };
        let Value::List(list) = heap.get(*key).unwrap() else {
            return NULL;
        };
        Value::Num(list.len() as f64)
    }
    pub fn list_pop_at(
        _: &mut Scope,
        args: Vec<Value>,
        heap: &mut SlotMap<RefKey, Value>,
    ) -> Value {
        let [Value::Ref(key), Value::Num(og_index)] = &args[..2] else {
            return NULL;
        };
        let index = og_index.floor() as usize;
        let Value::List(mut list) = heap.get(*key).unwrap().clone() else {
            return NULL;
        };
        list.remove(index);
        heap[*key] = Value::List(list.clone());
        Value::Ref(*key)
    }
    pub fn list_push(_: &mut Scope, args: Vec<Value>, heap: &mut SlotMap<RefKey, Value>) -> Value {
        let [Value::Ref(key), val] = &args[..2] else {
            return NULL;
        };
        let Value::List(mut list) = heap.get(*key).unwrap().clone() else {
            return NULL;
        };
        list.push(val.clone());
        heap[*key] = Value::List(list.clone());
        Value::Ref(*key)
    }
}
pub fn str_struct() -> Struct {
    use string_methods::*;
    let env = vars![
        parse_num(parse_num, 1),
        substr(substr, 3),
        len(len, 1),
        remove(remove, 2),
        replace(replace, 2),
        char_at(char_at, 2),
        split(split, -1)
    ];
    Struct {
        id: None,
        env: Scope::new(None, env, HashMap::from([])),
    }
}
pub(super) mod string_methods {
    use super::*;
    pub fn remove(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        let Value::Str(ref mut value) = args[0].clone() else {
            return NULL;
        };
        let target = &args[1];
        match target {
            Value::Num(index) => {
                if index < &0.0 || index >= &(value.len() as f64) {
                    return NULL;
                }
                value.remove(*index as usize);
                return Value::Str(value.to_string());
            }
            Value::Str(ref pattern) => return Value::Str(value.replace(pattern, "")),
            _ => NULL,
        }
    }

    pub fn replace(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        let [Value::Str(ref value), Value::Str(ref target), Value::Str(ref filler)] = args[..3]
        else {
            return NULL;
        };
        Value::Str(value.replace(target, filler))
    }
    pub fn substr(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        let [Value::Str(value), Value::Num(start), Value::Num(end)] = &args[..3] else {
            return NULL;
        };
        let start_index = start.floor() as usize;
        let end_index = end.floor() as usize;
        let sub = &value[start_index..end_index];
        Value::Str(sub.to_string())
    }
    pub fn len(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        let Value::Str(value) = &args[0] else {
            return NULL;
        };
        Value::Num(value.len() as f64)
    }

    pub fn char_at(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        let Value::Str(value) = &args[0] else {
            return NULL;
        };
        let Value::Num(index) = &args[1] else {
            return NULL;
        };

        value
            .chars()
            .nth(*index as usize)
            .map(|c| Value::Str(c.to_string()))
            .unwrap_or(Value::Null)
    }
    pub fn split(_: &mut Scope, args: Vec<Value>, heap: &mut SlotMap<RefKey, Value>) -> Value {
        let Value::Str(value) = &args[0] else {
            return NULL;
        };
        match args.len() {
            1 => {
                let split_str = value
                    .split(' ')
                    .map(|v| Value::Str(String::from(v)))
                    .collect();
                let val = heap.insert(Value::List(split_str));
                Value::Ref(val)
            }
            _ => {
                let Value::Str(seperator) = &args[1] else {
                    return NULL;
                };
                let split_str = value
                    .split(seperator)
                    .map(|v| Value::Str(String::from(v)))
                    .collect();
                let val = heap.insert(Value::List(split_str));
                Value::Ref(val)
            }
        }
    }
}
pub(super) mod functions {
    use super::*;
    pub fn delete_var(
        scope: &mut Scope,
        args: Vec<Value>,
        _: &mut SlotMap<RefKey, Value>,
    ) -> Value {
        let Value::Str(val) = &args[0] else {
            return NULL;
        };
        if !scope.vars.contains_key(val) {
            return NULL;
        }
        scope.vars.remove(val);
        NULL
    }

    pub fn println_builtin(
        _: &mut Scope,
        args: Vec<Value>,
        heap: &mut SlotMap<RefKey, Value>,
    ) -> Value {
        if args.is_empty() {
            println!();
        }
        let mut out = "".to_string();
        for val in args {
            out += format!(" {}", deref_val(val, heap)).as_str();
        }
        out = out.trim().to_string();
        println!("{out}");
        Value::Null
    }
    pub fn print_builtin(
        _: &mut Scope,
        args: Vec<Value>,
        heap: &mut SlotMap<RefKey, Value>,
    ) -> Value {
        if args.is_empty() {
            print!("");
            io::stdout().flush().unwrap();
            return Value::Null;
        }
        let mut out = "".to_string();
        for val in args {
            out += format!(" {}", deref_val(val, heap)).as_str();
        }
        out = out.trim().to_string();
        print!("{out}");
        io::stdout().flush().unwrap();
        NULL
    }
    pub fn open_textfile(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        let Value::Str(path) = &args[0] else {
            return NULL;
        };
        let Ok(contents) = fs::read_to_string(path) else {
            return NULL;
        };
        return Value::Str(contents);
    }
    pub fn typeof_node(
        _: &mut Scope,
        args: Vec<Value>,
        heap: &mut SlotMap<RefKey, Value>,
    ) -> Value {
        let val = if let Value::Ref(id) = args[0].clone() {
            &heap[id]
        } else {
            &args[0]
        };
        let out = val.get_type().to_string();
        Value::Str(out)
    }
    pub fn parse_num(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        let Value::Str(input) = &args[0] else {
            return NULL;
        };
        Value::Num(input.parse().unwrap())
    }
    pub fn min(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        let [Value::Num(val1), Value::Num(val2)] = &args[..2] else {
            return NULL;
        };
        Value::Num(val1.min(*val2))
    }
    pub fn max(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        let [Value::Num(val1), Value::Num(val2)] = &args[..2] else {
            return NULL;
        };
        Value::Num(val1.max(*val2))
    }
    pub fn pow(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        let [Value::Num(val1), Value::Num(val2)] = &args[..2] else {
            return NULL;
        };
        Value::Num(val1.powf(*val2))
    }
    pub fn cos(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        let Value::Num(val1) = &args[0] else {
            return NULL;
        };
        Value::Num(val1.cos())
    }
    pub fn tan(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        let Value::Num(val1) = &args[0] else {
            return NULL;
        };
        Value::Num(val1.tan())
    }
    pub fn sin(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        let Value::Num(val1) = &args[0] else {
            return NULL;
        };
        Value::Num(val1.sin())
    }
    pub fn sqrt(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        let Value::Num(val1) = &args[0] else {
            return NULL;
        };
        Value::Num(val1.sqrt())
    }
    pub fn floor(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        let Value::Num(val1) = &args[0] else {
            return NULL;
        };
        Value::Num(val1.floor())
    }
    pub fn round(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        let Value::Num(val1) = &args[0] else {
            return NULL;
        };
        Value::Num(val1.round())
    }
    pub fn deref_val(val: Value, heap: &SlotMap<RefKey, Value>) -> Value {
        if let Value::Ref(id) = val {
            return heap[id].clone();
        }
        val
    }
    pub fn to_str(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        Value::Str(args[0].to_string())
    }
    pub fn stringify_vals(
        _: &mut Scope,
        args: Vec<Value>,
        _: &mut SlotMap<RefKey, Value>,
    ) -> Value {
        if args.is_empty() {
            return Value::Str("".to_owned());
        }
        let mut out = String::new();
        for val in args {
            out += (val.to_string() + "").as_str();
        }
        Value::Str(out.trim().to_owned())
    }
    pub fn unix_time(_: &mut Scope, _: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        Value::Num(
            SystemTime::now()
                .duration_since(time::UNIX_EPOCH)
                .unwrap()
                .as_secs_f64(),
        )
    }
    pub fn wait_builtin(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        let Value::Num(val1) = &args[0] else {
            return NULL;
        };
        thread::sleep(Duration::from_millis((val1 * 1000.0).floor() as u64));
        NULL
    }
    pub fn input_builtin(_: &mut Scope, args: Vec<Value>, _: &mut SlotMap<RefKey, Value>) -> Value {
        if !args.is_empty() {
            print!("{}", args[0]);
            io::stdout().flush().unwrap();
        }
        let mut result = String::new();
        let read = io::stdin().read_line(&mut result);
        if read.is_err() {
            result = "".to_string()
        }
        return Value::Str(String::from(result.trim()));
    }
    pub fn eval(scope: &mut Scope, args: Vec<Value>, heap: &mut SlotMap<RefKey, Value>) -> Value {
        let Value::Str(source) = &args[0] else {
            return NULL;
        };
        let mut parser = Parser::new(source.as_str());
        let ast_result = parser.parse();
        let Ok((ast, functions)) = ast_result else {
            return Value::Null;
        };
        let mut inter = Interpreter::new(ast, functions);
        inter.heap.clone_from(heap);
        let Ok(result) = inter.execute_with(&mut scope.clone()) else {
            return Value::Null;
        };

        result
    }
    pub fn import_var(
        parent: &mut Scope,
        args: Vec<Value>,
        heap: &mut SlotMap<RefKey, Value>,
    ) -> Value {
        let Value::Str(path) = &args[0] else {
            return NULL;
        };
        let Ok(source) = fs::read_to_string(path) else {
            return NULL;
        };
        let mut parser = Parser::new(source.as_str());
        let Ok((ast, functions)) = parser.parse() else {
            return NULL;
        };
        let mut inter = Interpreter::new(ast, functions);
        let base = Scope::from_vars(vars!(
            __name__ => Value::Str("lib".to_string())
        ));
        let Ok(scope) = inter.parse_vars(base) else {
            return NULL;
        };
        let heapstuff = |(name, val): (&String, &Value)| -> (String, Value) {
            let mut new_val = val.to_owned();
            if let Value::Ref(id) = val {
                let derefed = &inter.heap[*id];
                let key = heap.insert(derefed.clone());
                new_val = Value::Ref(key);
            }
            (name.to_owned(), new_val)
        };
        let new_vars: HashMap<_, _> = parent
            .vars
            .iter()
            .chain(&scope.vars)
            .map(heapstuff)
            .collect();
        let new_structs: HashMap<_, _> = parent
            .structs
            .iter()
            .chain(&scope.structs)
            .map(|(k, v)| (k.to_owned(), v.to_owned()))
            .collect();
        parent.vars = new_vars;
        parent.structs = new_structs;
        Value::Null
    }
    fn gen_range(from: f64, to: f64, inc: f64) -> Vec<Value> {
        let capacity = from.round().abs() as usize + to.round().abs() as usize;
        let mut buffer: Vec<Value> = Vec::with_capacity(capacity);
        let mut current_num = from;
        if to > 0.0 {
            while current_num < to {
                buffer.push(Value::Num(current_num));
                current_num += inc;
            }
        } else {
            while current_num > to {
                buffer.push(Value::Num(current_num));
                current_num += inc;
            }
        }
        buffer
    }
    pub fn range(_: &mut Scope, args: Vec<Value>, heap: &mut SlotMap<RefKey, Value>) -> Value {
        match args.len() {
            0 => {
                return Value::Null;
            }
            1 => {
                let Value::Num(to) = args[0] else { return NULL };
                let new_range = gen_range(0.0, to, 1.0);
                let val = heap.insert(Value::List(new_range));
                Value::Ref(val)
            }
            2 => {
                let Value::Num(from) = args[0] else {
                    return NULL;
                };
                let Value::Num(to) = args[1] else { return NULL };
                let new_range = gen_range(from, to, 1.0);
                let val = heap.insert(Value::List(new_range));
                Value::Ref(val)
            }
            _ => {
                let Value::Num(from) = args[0] else {
                    return NULL;
                };
                let Value::Num(to) = args[1] else { return NULL };
                let Value::Num(inc) = args[2] else {
                    return NULL;
                };
                if inc == 0.0 {
                    return NULL;
                }
                if from == 0.0 && to == 0.0 {
                    return NULL;
                }
                if from >= 0.0 && inc > 0.0 && to < 0.0 {
                    return NULL;
                }
                if from <= 0.0 && inc < 0.0 && to > 0.0 {
                    return NULL;
                }
                let new_range = gen_range(from, to, inc);
                let val = heap.insert(Value::List(new_range));
                Value::Ref(val)
            }
        }
    }
}
