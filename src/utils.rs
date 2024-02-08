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
