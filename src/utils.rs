use std::fmt::{Debug, Formatter};

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
/// <!-- rust-analyzer delimiter nudge
/// '''rust ,ignore
/// hashmap![];
/// ''' -->
macro_rules! hashmap {
    [] => {
        HashMap::new()
    };
    [<$ty1:ty,$ty2:ty>] => {
        HashMap::<$ty1,$ty2>::new()
    };
    [<$ty2:ty>$($key:ident => $val:expr),*] => {
        HashMap::<String,$ty2>::from([
        $(
            (stringify!($key).to_string(),$val),
        )*
        ])
    };
    [<$ty1:ty,$ty2:ty>$($key:expr => $val:expr),*] => {
        HashMap::<$ty1,$ty2>::from([
        $(
            ($key,$val),
        )*
        ])
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
#[macro_export]
macro_rules! _test_func {
    (,) => {};
    ({$($tt:tt)*}) => {
        stringify!($($tt)*)
    };
    ($lit:literal) => {
        $lit
    };
}

pub fn compact_iter_debug<T: Iterator>(fmt: &mut Formatter, iter: T) -> std::fmt::Result
where
	T::Item: Debug,
{
	writeln!(fmt, "[")?;
	for i in iter {
		writeln!(fmt, "    {i:?},")?;
	}
	write!(fmt, "]")
}
