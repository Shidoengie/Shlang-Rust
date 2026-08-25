use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields, parse_macro_input};

#[proc_macro_derive(VariantName)]
pub fn derive_variant_name(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as DeriveInput);
	let name = &input.ident;
	let data = match &input.data {
		Data::Enum(data) => data,
		_ => {
			return syn::Error::new_spanned(name, "VariantName can only be derived for enums")
				.to_compile_error()
				.into();
		}
	};
	let arms = data.variants.iter().map(|variant| {
		let var_ident = &variant.ident;
		let var_str = var_ident.to_string();
		match &variant.fields {
			Fields::Unit => {
				quote! { #name::#var_ident => #var_str }
			}
			Fields::Unnamed(_) => {
				quote! { #name::#var_ident(..) => #var_str }
			}
			Fields::Named(_) => {
				quote! { #name::#var_ident { .. } => #var_str }
			}
		}
	});

	// Handle generic types on the enum (e.g., MyEnum<T>)
	let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

	// Generate the impl block
	let expanded = quote! {
		impl #impl_generics #name #ty_generics #where_clause {
			/// Returns the name of the enum variant as a static string slice.
			pub fn variant_name(&self) -> &'static str {
				match self {
					#(#arms,)*
				}
			}
		}
	};

	TokenStream::from(expanded)
}
