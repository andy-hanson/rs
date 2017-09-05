extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use syn::{Ident, Body, VariantData, Field, Ty, PathParameters};
use proc_macro::TokenStream;
use quote::Tokens;

#[proc_macro_derive(NoDrop)]
pub fn no_drop(input: TokenStream) -> TokenStream {
	let s = input.to_string();
	let ast = syn::parse_derive_input(&s).unwrap();
	let gen = impl_no_drop(&ast);
	gen.parse().unwrap()
}

fn impl_no_drop(ast: &syn::DeriveInput) -> Tokens {
	let self_name = &ast.ident;

	let mut idents = Vec::<Tokens>::new();
	check_body(&ast.body, self_name, &mut idents);

	let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

	let isses = idents.iter().map(|ident| quote! { #ident::NO_DROP_MARKER });
	let is = if isses.len() == 0 { quote! { 0 } } else { quote! { #(#isses)+* } };
	let res = quote! {
		impl #impl_generics NoDrop for #self_name #ty_generics #where_clause {
			const NO_DROP_MARKER: u8 = #is;
		}
	};
	//println!("{}", res);
	return res
}

fn check_body(body: &Body, self_name: &Ident, idents: &mut Vec<Tokens>) {
	match *body {
		Body::Struct(ref vd) => check_variant(vd, self_name, idents),
		Body::Enum(ref body) => {
			for case in body.iter() {
				check_variant(&case.data, self_name, idents)
			}
		}
	}
}

fn check_variant(vd: &VariantData, self_name: &Ident, idents: &mut Vec<Tokens>) {
	match *vd {
		VariantData::Struct(ref body) => check_fields(body, self_name, idents),
		VariantData::Tuple(ref body) => check_fields(body, self_name, idents),
		VariantData::Unit => {}, // OK
	}
}

fn check_fields(body: &Vec<Field>, self_name: &Ident, idents: &mut Vec<Tokens>) {
	for field in body.iter() {
		check_ty(&field.ty, self_name, &mut Some(idents))
	}
}

fn check_ty(ty: &Ty, self_name: &Ident, idents: &mut Option<&mut Vec<Tokens>>) {
	match *ty {
		Ty::Slice(ref inner) => check_ty(inner, self_name, idents),
		Ty::Array(ref inner, _) => check_ty(inner, self_name, idents),
		Ty::Ptr(_) => unimplemented!("Ptr"),
		Ty::Rptr(_, _) => {}, // References always OK, regardless of what's in the reference
		Ty::BareFn(_) => unimplemented!("BareFn"),
		Ty::Never => unimplemented!("Never"),
		Ty::Tup(ref tys) => {
			for inner in tys.iter() {
				check_ty(inner, self_name, idents)
			}
		}
		Ty::Path(_, ref path) => {
			let foo = path.segments.iter().map(|segment| {
				match segment.parameters {
					PathParameters::AngleBracketed(ref ab) => {
						for inner in &ab.types {
							check_ty(inner, self_name, idents)
						}
						for _ in &ab.bindings {
							unimplemented!()
						}
						let id = &segment.ident;
						if !ab.lifetimes.is_empty() {
							let lifetimes = &ab.lifetimes;
							if !ab.types.is_empty() {
								let types = &ab.types;
								quote! { #id::<#(#lifetimes),*, #(#types),*> }
							} else {
								quote! { #id::<#(#lifetimes),*> }
							}
						} else {
							if !ab.types.is_empty() {
								let types = &ab.types;
								quote! { #id::<#(#types),*> }
							} else {
								quote! { #id }
							}
						}
					}
					PathParameters::Parenthesized(_) => unimplemented!(),
				}
			}).collect::<Vec<_>>();
			if let Some(ref mut id) = *idents {
				// Avoid recursively accessing Foo::NO_DROP_MARKER when inside of `Foo`
				if path.segments.last().unwrap().ident != self_name {
					id.push(quote! { #(#foo)::* });
				}
			}
		}
		Ty::TraitObject(_) => unimplemented!(),
		Ty::ImplTrait(_) => unimplemented!(),
		Ty::Paren(ref inner) => check_ty(inner, self_name, idents),
		Ty::Infer => unimplemented!(),
		Ty::Mac(_) => unimplemented!(),
	}
}
