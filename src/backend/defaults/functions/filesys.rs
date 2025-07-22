use super::*;
use std::fs;
pub fn open_textfile(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(Value::Str(path) = Type::Str;data,state);
    let Ok(contents) = fs::read_to_string(path) else {
        return Ok(create_err("Failed to open file", &mut state.heap));
    };
    return Ok(Value::Str(contents));
}
pub fn open_dir(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(Value::Str(path) = Type::Str;data,state);
    let contents = catch!(err {
        return Ok(create_err(err, &mut state.heap));
    } in fs::read_dir(path));
    let mut files: Vec<Value> = vec![];
    for query in contents {
        match query {
            Ok(entry) => {
                let Ok(file) = fs::read_to_string(entry.path()) else {
                    unimplemented!()
                };
                files.push(Value::Str(file));
            }
            Err(err) => {
                return Ok(create_err(err.to_string(), &mut state.heap));
            }
        }
    }
    return Ok(Value::Ref(state.heap.insert(Value::List(files))));
}
pub fn paths_in_dir(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(Value::Str(path) = Type::Str;data,state);
    let contents = catch!(err {
        return Ok(create_err(err, &mut state.heap));
    } in fs::read_dir(path));
    let mut paths: Vec<Value> = vec![];
    for query in contents {
        match query {
            Ok(entry) => {
                paths.push(Value::Str(entry.path().to_str().unwrap().to_owned()));
            }
            Err(err) => {
                return Ok(create_err(err.to_string(), &mut state.heap));
            }
        }
    }
    return Ok(Value::Ref(state.heap.insert(Value::List(paths))));
}
pub fn create_file(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Str(path) = Type::Str
    ;data,state);
    let Ok(_) = fs::File::create(path) else {
        return Ok(create_err("Failed to create file", &mut state.heap));
    };
    return NULL;
}
pub fn create_dir(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Str(path) = Type::Str
    ;data,state);
    let Ok(_) = fs::create_dir(path) else {
        return Ok(create_err("Failed to create directory", &mut state.heap));
    };
    return NULL;
}
pub fn delete_dir(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Str(path) = Type::Str
    ;data,state);
    if let Err(err) = fs::remove_dir_all(path) {
        return Ok(create_err(err.to_string(), &mut state.heap));
    }
    return NULL;
}
pub fn delete_file(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Str(path) = Type::Str
    ;data,state);
    if let Err(err) = fs::remove_file(path) {
        return Ok(create_err(err.to_string(), &mut state.heap));
    }
    return NULL;
}
pub fn write_file(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Str(path) = Type::Str,
        Value::Str(contents) = Type::Str
    ;data,state);
    let Ok(_) = fs::write(path, contents) else {
        return Ok(create_err("Failed to write file", &mut state.heap));
    };
    return NULL;
}
pub fn new_file(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Str(path) = Type::Str,
        Value::Str(contents) = Type::Str
    ;data,state);
    let Ok(_) = fs::write(path, contents) else {
        return Ok(create_err("Failed to write file", &mut state.heap));
    };
    return NULL;
}
