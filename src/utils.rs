#[macro_export]
macro_rules! catch {
    ($name:ident $fail:block in $val:expr) => {
        match $val {
            Ok(ok) => ok,
            Err($name) => $fail,
        }
    };
}

#[macro_export]
macro_rules! bx {
    ($e:expr) => {
        Box::new($e)
    };
}
#[macro_export]
macro_rules! hashmap {
    [] => {
        HashMap::new()
    };

    [$($key:ident => $val:expr),*] => {
        HashMap::from([
        $(
            (stringify!($key).to_string(),$val),
        )*
        ])
    };
    [$($key:expr => $val:expr),*] => {
        HashMap::from([
        $(
            ($key,$val),
        )*
        ])
    };

}
#[macro_export]
macro_rules! char_vec {
    ($chars:expr) => {{
        let val: Vec<char> = $chars.par_chars().collect();
        CharVec(val)
    }};
}
