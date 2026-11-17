use std::{
	fs,
	io::{Error, ErrorKind, Result},
	path::{Path, PathBuf},
};

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use regex::Regex;
use syn::{
	Expr, ExprLit, Ident, Lit, LitStr, Token,
	parse::{Parse, ParseStream, Parser},
	parse_macro_input,
	spanned::Spanned,
};

struct TestFileInput {
	section: Ident,
	function: Expr,
	path: Expr,
}

impl Parse for TestFileInput {
	fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
		let section = input.parse()?;
		input.parse::<Token![,]>()?;
		let function = input.parse()?;
		input.parse::<Token![,]>()?;
		let path = input.parse()?;
		let _ = input.parse::<Option<Token![,]>>()?;

		if !input.is_empty() {
			return Err(input.error("unexpected tokens after test file path"));
		}

		Ok(Self {
			section,
			function,
			path,
		})
	}
}

fn string_value(expression: &Expr) -> syn::Result<String> {
	match expression {
		Expr::Lit(ExprLit {
			lit: Lit::Str(value),
			..
		}) => Ok(value.value()),
		Expr::Paren(paren) => string_value(&paren.expr),
		Expr::Group(group) => string_value(&group.expr),
		Expr::Macro(expression) if expression.mac.path.is_ident("concat") => {
			let parts = syn::punctuated::Punctuated::<Expr, Token![,]>::parse_terminated
				.parse2(expression.mac.tokens.clone())?;
			parts.iter().try_fold(String::new(), |mut output, part| {
				output.push_str(&string_value(part)?);
				Ok(output)
			})
		}
		_ => Err(syn::Error::new(
			expression.span(),
			"test file path must be a string literal or concat! of string literals",
		)),
	}
}

fn parse_test_file(path: &Path) -> Result<Vec<(String, String)>> {
	let delimiter = Regex::new(r"^//(?<name>.+?)\s*$").unwrap();
	let contents = fs::read_to_string(path)?;
	let mut tests = Vec::new();
	let mut current: Option<(String, String)> = None;

	for line in contents.lines() {
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

#[proc_macro]
pub fn test_file(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as TestFileInput);
	let path_value = match string_value(&input.path) {
		Ok(path) => path,
		Err(error) => return error.to_compile_error().into(),
	};
	let base_path = match std::env::var_os("CARGO_MANIFEST_DIR") {
		Some(path) => PathBuf::from(path),
		None => {
			return syn::Error::new(input.path.span(), "CARGO_MANIFEST_DIR is not set")
				.to_compile_error()
				.into();
		}
	};
	let path = PathBuf::from(path_value);
	let path = if path.is_absolute() {
		path
	} else {
		base_path.join(path)
	};

	let tests = match parse_test_file(&path) {
		Ok(tests) => tests,
		Err(error) => {
			return syn::Error::new(input.path.span(), error.to_string())
				.to_compile_error()
				.into();
		}
	};

	let section = input.section;
	let function = input.function;
	let generated_tests = tests.iter().map(|(name, source)| {
		let test_name = name.to_lowercase().replace(" ", "_");
		let test_ident = format_ident!("{}_{}", section, test_name);
		let name = LitStr::new(name, input.path.span());
		let source = LitStr::new(source, input.path.span());

		quote! {
			#[test]
			fn #test_ident() {
				let name = #name;
				let source = #source;

				insta::with_settings!(
					{ info => &stringify!(#section), sort_maps => true },
					{
						insta::with_settings!(
							{ description => &crate::testing::format_test_description(name, source) },
							{
								let result = #function(source);
								if let Ok(source) = result {
									insta::assert_debug_snapshot!(name, source);
								} else {
									insta::assert_debug_snapshot!(name, result);
								}

							}
						);
					}
				);
			}
		}
	});

	quote!(#(#generated_tests)*).into()
}
