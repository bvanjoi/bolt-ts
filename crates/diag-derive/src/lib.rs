use quote::quote;
use syn::{Attribute, DeriveInput, parse_macro_input};

fn find_field_satisfy_attr(
    input: &DeriveInput,
    attr_f: impl Fn(&Attribute) -> bool,
) -> Option<syn::Ident> {
    if let syn::Data::Struct(data_struct) = &input.data {
        if let syn::Fields::Named(fields_named) = &data_struct.fields {
            for field in fields_named.named.iter() {
                for attr in &field.attrs {
                    if attr_f(attr) {
                        if let Some(ident) = &field.ident {
                            return Some(ident.clone());
                        }
                    }
                }
            }
        }
    }
    None
}

enum DiagnosticExt {
    Struct {
        name: syn::Ident,
        primary_field_name: syn::Ident,
        related_field_name: Option<syn::Ident>,
    },
    Enum {
        name: syn::Ident,
        variant_names: Vec<syn::Ident>,
    },
}

impl DiagnosticExt {
    fn from_derive_input(input: DeriveInput) -> Self {
        match input.data {
            syn::Data::Struct(_) => {
                let primary_field_name = find_field_satisfy_attr(&input, |attr| {
                    attr.meta.require_list().is_ok_and(|list| {
                        list.path.is_ident("label") && list.tokens.to_string() == "primary"
                    })
                })
                .expect("`#[label(primary)]` not found.");
                let related_field_name =
                    find_field_satisfy_attr(&input, |attr| attr.path().is_ident("related"));
                Self::Struct {
                    name: input.ident,
                    primary_field_name,
                    related_field_name,
                }
            }
            syn::Data::Enum(syn::DataEnum { variants, .. }) => {
                let variant_names = variants
                    .iter()
                    .map(|variant| variant.ident.clone())
                    .collect();
                Self::Enum {
                    name: input.ident,
                    variant_names,
                }
            }
            _ => unimplemented!(),
        }
    }

    fn r#gen(self) -> proc_macro2::TokenStream {
        match self {
            Self::Struct {
                name,
                primary_field_name,
                related_field_name,
            } => {
                if let Some(related_name) = related_field_name {
                    quote! {
                        impl diag_ext::DiagnosticExt for #name {
                            fn module_id(&self) -> bolt_ts_span::ModuleID {
                                self.#primary_field_name.module
                            }

                            fn steal_related(&mut self) -> Option<Vec<Box<dyn diag_ext::DiagnosticExt + Send + Sync + 'static>>> {
                                let old = std::mem::take(&mut self.#related_name);
                                Some(old.into_iter().map(|x| Box::new(x) as Box<dyn diag_ext::DiagnosticExt + Send + Sync +'static>).collect())
                            }
                        }
                    }
                } else {
                    quote! {
                        impl diag_ext::DiagnosticExt for #name {
                            fn module_id(&self) -> bolt_ts_span::ModuleID {
                                self.#primary_field_name.module
                            }

                            fn steal_related(&mut self) -> Option<Vec<Box<dyn diag_ext::DiagnosticExt + Send + Sync + 'static>>> {
                                None
                            }
                        }
                    }
                }
            }
            Self::Enum {
                name,
                variant_names,
            } => {
                let module_id_arms = variant_names.iter().map(|name| {
                    quote! {
                        Self::#name(x) => x.module_id(),
                    }
                });
                let steal_related_arms = variant_names.iter().map(|name| {
                    quote! {
                        Self::#name(x) => x.steal_related(),
                    }
                });

                quote! {
                    impl diag_ext::DiagnosticExt for #name {
                        fn module_id(&self) -> bolt_ts_span::ModuleID {
                            match self {
                                #(#module_id_arms)*
                            }
                        }
                        fn steal_related(&mut self) -> Option<Vec<Box<dyn diag_ext::DiagnosticExt + Send + Sync +'static>>> {
                            match self {
                                #(#steal_related_arms)*
                            }
                        }
                    }
                }
            }
        }
    }
}

#[proc_macro_derive(DiagnosticExt, attributes(label, related))]
pub fn derive_diagnostic_ext(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let stream = DiagnosticExt::from_derive_input(input).r#gen();

    proc_macro::TokenStream::from(stream)
}
