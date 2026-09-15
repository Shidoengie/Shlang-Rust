use std::{
	fs,
	io::{Error, ErrorKind, Result},
	path::Path,
};

use regex::Regex;

pub fn test_file(path: impl AsRef<Path>) -> Result<Vec<(String, String)>> {
	let path = path.as_ref();
	let delimiter = Regex::new(r"^//(?<name>.+?)\s*$").unwrap();
	let mut tests = vec![];
	let mut current: Option<(String, String)> = None;

	for line in fs::read_to_string(path)?.lines() {
		if let Some(captures) = delimiter.captures(line) {
			if let Some((name, source)) = current.take() {
				tests.push((name, source.trim().to_owned()));
			}
			current = Some((captures["name"].trim().to_owned(), String::new()));
		} else if let Some((_, source)) = current.as_mut() {
			source.push_str(line);
			source.push('\n');
		} else if !line.trim().is_empty() {
			return Err(Error::new(
				ErrorKind::InvalidData,
				format!(
					"test data in {} must begin with a //case_name delimiter",
					path.display()
				),
			));
		}
	}

	if let Some(test) = current {
		tests.push(test);
	}
	if tests.is_empty() {
		return Err(Error::new(
			ErrorKind::InvalidData,
			format!("test data in {} contains no cases", path.display()),
		));
	}
	Ok(tests)
}

#[macro_export]
macro_rules! test_func {
    (
        $(  $section:ident, $func:expr,
            {$($name:expr => $test:tt $(,)?)* }
        $(,)?)*
    ) => {

        $(
            #[test]
            fn $section() {
                use crate::_test_func;
                insta::with_settings!(
                {description => stringify!($section),sort_maps => true},
                {
                    $(
                        let _ = std::panic::catch_unwind(||{
                        insta::assert_debug_snapshot!($name,$func(_test_func!($test)));
                        }).inspect_err(|err| println!("{err:?}"));
                    )*
                }

            )
        }
        )*
    };

}
pub fn format_test_description(name: &str, source: &str) -> String {
	let source = source.trim();
	format!("Test: {name}\n{:─<82}\n{source}", "")
}
#[macro_export]
macro_rules! test_file {
	($section:ident, $func:expr, $path:expr $(,)?) => {
		#[test]
		fn $section() {

			insta::with_settings!(
				{ info => &stringify!($section), sort_maps => true},
				{
					for (name, source) in crate::testing::test_file($path).unwrap() {

						insta::with_settings!(
							{ description => &crate::testing::format_test_description(&name,&source)},
							{
								insta::assert_debug_snapshot!(name, $func(&source));
							}
						);
					}
				}
			)
		}
	};
}
