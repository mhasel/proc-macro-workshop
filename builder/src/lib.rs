use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{parse_macro_input, DeriveInput, GenericArgument, PathArguments, Type};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_type_name = Ident::new(&format!("{}Builder", name.to_string()), Span::call_site());

    let syn::Data::Struct(struct_data) = input.data else {
        todo!("error handling")
    };

    let fields = match struct_data.fields {
        syn::Fields::Named(named_fields) => named_fields.named,
        _ => unreachable!("is it really tho"),
    };

    let (idents, types): (Vec<_>, Vec<_>) = fields
        .into_iter()
        .filter(|field| field.ident.is_some())
        .map(|field| (field.ident.unwrap(), field.ty))
        .unzip();

    let (inner_types, is_option): (Vec<_>, Vec<_>) =
        types.iter().map(|ty| try_get_inner(ty)).unzip();

    let assignments = idents
        .iter()
        .zip(is_option.iter())
        .map(|(id, opt)| {
            if *opt {
                quote!(
                    #id: self.#id.clone()
                )
            } else {
                quote!(
                    #id: if let Some(field) = &self.#id {
                        field.clone()
                    } else {
                        return Err(String::from("Not all fields set").into())
                    }
                )
            }
        })
        .collect::<Vec<_>>();

    let expanded = quote!(
        use std::error::Error;

        pub struct #builder_type_name {
            #(#idents : Option<#inner_types>),*
        }

        impl #name {
            pub fn builder() -> #builder_type_name {
                #builder_type_name {
                    #(#idents: None),*
                }
            }
        }

        impl #builder_type_name {
            #(
                pub fn #idents(&mut self, #idents: #inner_types) -> &mut Self {
                    self.#idents = Some(#idents);
                    self
            })*

            pub fn build(&mut self) -> Result<#name, Box<dyn Error>> {
                Ok(
                    #name {
                        #(#assignments,)*
                    }
                )
            }
        }
    );

    TokenStream::from(expanded)
}

/// Checks if type is an Option<T> and returns a Tuple consisting of the inner type and
/// a boolean, which is used as an `is_option` flag.
fn try_get_inner(ty: &Type) -> (&Type, bool) {
    let Type::Path(type_path) = &ty else {
        return (ty, false)
    };

    let Some(segment) = type_path.path.segments.first() else {
        unreachable!("segments arent optional")
    };

    if !(segment.ident == Ident::new("Option", Span::call_site())) {
        return (ty, false);
    }

    let PathArguments::AngleBracketed(inner) = &segment.arguments else {
        unreachable!("expected the default option syntax")
    };

    let Some(GenericArgument::Type(ty)) = inner.args.first() else {
        unreachable!("option must have a type argument")
    };

    (ty, true)
}
