use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput, Error};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    builder::expand(input)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

mod builder {
    use proc_macro2::TokenStream;
    use proc_macro2::{Ident, Span};
    use quote::quote;
    use syn::{DeriveInput, GenericArgument, Meta, PathArguments, Result, Type, Expr};

    pub(crate) fn expand(input: DeriveInput) -> Result<TokenStream> {
        let name = input.ident;
        let builder_type_name =
            Ident::new(&format!("{}Builder", name.to_string()), Span::call_site());

        let syn::Data::Struct(struct_data) = input.data else {
            todo!("error handling")
        };

        let (fields, attributes): (Vec<_>, Vec<_>) = match struct_data.fields {
            syn::Fields::Named(named_fields) => named_fields.named,
            _ => todo!("error handling"), // a builder doesn't make sense on any other struct variant (I think?)
        }
        .into_iter()
        .filter(|it| it.ident.is_some())
        .map(|field| ((field.ident.unwrap(), field.ty), field.attrs))
        .unzip();

        let (field_id, types): (Vec<_>, Vec<_>) = fields.into_iter().unzip();

        let (inner_types, is_option): (Vec<_>, Vec<_>) =
            types.iter().map(|ty| try_get_inner(ty)).unzip();

        // construct all field assignments.
        // if the field is not optional and also has not been set, return an error
        let assignments = field_id
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

        let setters = quote!(
            #(
                pub fn #field_id(&mut self, #field_id: #inner_types) -> &mut Self {
                    self.#field_id = Some(#field_id);
                    self
            })*
        );

        let _setters = field_id.iter().zip(attributes.iter()).map(|(id, attr)| {
            if let Some(attr) = attr.first() {
                let Meta::NameValue(mnv) = &attr.meta else {
                    todo!()     
                };
                
                let Expr::Lit(exprlit) = &mnv.value else {
                    todo!()
                };

                let syn::Lit::Str(value) = &exprlit.lit else {
                    todo!()
                };

                if id.to_string() == value.value() {
                    // only generate one-at-a-time method
                    todo!()
                } else {
                    // generate all-at-once method and one-at-a-time method
                    todo!()
                }

            } else {
                todo!()
            }
        });

        let expanded = quote!(
            use std::error::Error;

            pub struct #builder_type_name {
                #(#field_id : Option<#inner_types>),*
            }

            impl #name {
                pub fn builder() -> #builder_type_name {
                    #builder_type_name {
                        #(#field_id: None),*
                    }
                }
            }

            impl #builder_type_name {
                #setters

                pub fn build(&mut self) -> Result<#name, Box<dyn Error>> {
                    Ok(
                        #name {
                            #(#assignments,)*
                        }
                    )
                }
            }
        );

        Ok(TokenStream::from(expanded))
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
}
