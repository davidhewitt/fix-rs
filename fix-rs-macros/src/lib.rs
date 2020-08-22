// Copyright 2017 James Bendig. See the COPYRIGHT file at the top-level
// directory of this distribution.
//
// Licensed under:
//   the MIT license
//     <LICENSE-MIT or https://opensource.org/licenses/MIT>
//   or the Apache License, Version 2.0
//     <LICENSE-APACHE or https://www.apache.org/licenses/LICENSE-2.0>,
// at your option. This file may not be copied, modified, or distributed
// except according to those terms.

#![crate_type = "proc-macro"]
#![recursion_limit = "256"]

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;

enum ExtractAttributeError {
    BodyNotStruct,
    FieldNotFound,
    AttributeNotFound,
    AttributeNotNameValue,
    AttributeValueWrongType,
}

fn extract_attribute_value(ast: &syn::DeriveInput,field_ident: &'static str,attr_ident: &'static str) -> Result<syn::Lit,ExtractAttributeError> {
    if let syn::Data::Struct(ref data) = ast.data {
        if let syn::Fields::Named(fields) = &data.fields {
            for field in &fields.named {
                if field.ident.as_ref().expect("Field must have an identifier") != field_ident {
                    continue;
                }

                for attr in &field.attrs {
                    if !attr.path.is_ident(attr_ident) {
                        continue;
                    }

                    if let Ok(syn::Meta::NameValue(nv)) = attr.parse_meta() {
                        return Ok(nv.lit);
                    }
                    else {
                        return Err(ExtractAttributeError::AttributeNotNameValue);
                    }
                }

                return Err(ExtractAttributeError::AttributeNotFound);
            }

            return Err(ExtractAttributeError::FieldNotFound);
        }
    }

    Err(ExtractAttributeError::BodyNotStruct)
}

fn extract_attribute_byte_str(ast: &syn::DeriveInput,field_ident: &'static str,attr_ident: &'static str) -> Result<Vec<u8>,ExtractAttributeError> {
    let lit = extract_attribute_value(ast,field_ident,attr_ident)?;

    if let syn::Lit::ByteStr(bytes) = lit {
       return Ok(bytes.value());
    }

    Err(ExtractAttributeError::AttributeValueWrongType)
}

fn extract_attribute_int(ast: &syn::DeriveInput,field_ident: &'static str,attr_ident: &'static str) -> Result<u64,ExtractAttributeError> {
    let lit = extract_attribute_value(ast,field_ident,attr_ident)?;

    if let syn::Lit::Int(value) = lit {
        if let Ok(val) = value.base10_parse() {
            return Ok(val);
        }
    }

    Err(ExtractAttributeError::AttributeValueWrongType)
}

#[proc_macro_derive(BuildMessage,attributes(message_type))]
pub fn build_message(input: TokenStream) -> TokenStream {
    let source = input.to_string();
    let ast = syn::parse(input).unwrap();

    let message_type = match extract_attribute_byte_str(&ast,"_message_type_gen","message_type") {
        Ok(bytes) => bytes,
        Err(ExtractAttributeError::BodyNotStruct) => panic!("#[derive(BuildMessage)] can only be used with structs"),
        Err(ExtractAttributeError::FieldNotFound) => panic!("#[derive(BuildMessage)] requires a _message_type_gen field to be specified"),
        Err(ExtractAttributeError::AttributeNotFound) => Vec::new(),
        Err(ExtractAttributeError::AttributeNotNameValue) |
        Err(ExtractAttributeError::AttributeValueWrongType) => panic!("#[derive(BuildMessage)] message_type attribute must be a byte string value like #[message_type=b\"1234\"]"),
    };
    let is_fixt_message = source.contains("sender_comp_id") && source.contains("target_comp_id");

    //Setup symbols.
    let message_name = ast.ident;
    let build_message_name = syn::Ident::new(&format!("Build{}", message_name), Span::call_site());
    let mut message_type_header = "35=".as_bytes().to_vec();
    message_type_header.extend_from_slice(&message_type);
    message_type_header.extend_from_slice(b"\x01");

    //Convert symbols into tokens so quote's ToTokens trait doesn't quote them.
    let message_type_header = syn::LitByteStr::new(&message_type_header, Span::call_site());

    let mut tokens = quote! {
        impl #message_name {
            fn msg_type_header() -> &'static [u8] {
                #message_type_header
            }
        }

        pub struct #build_message_name {
            cache: message::BuildMessageInternalCache,
        }

        impl #build_message_name {
            fn new() -> #build_message_name {
                #build_message_name {
                    cache: message::BuildMessageInternalCache {
                        fields_fix40: None,
                        fields_fix41: None,
                        fields_fix42: None,
                        fields_fix43: None,
                        fields_fix44: None,
                        fields_fix50: None,
                        fields_fix50sp1: None,
                        fields_fix50sp2: None,
                    },
                }
            }

            fn new_into_box() -> Box<message::BuildMessage + Send> {
                Box::new(#build_message_name::new())
            }
        }

        impl message::BuildMessage for #build_message_name {
            fn first_field(&self,version: message_version::MessageVersion) -> field_tag::FieldTag {
                #message_name::first_field(version)
            }

            fn field_count(&self,version: message_version::MessageVersion) -> usize {
                #message_name::field_count(version)
            }

            fn fields(&mut self,version: message_version::MessageVersion) -> message::FieldHashMap {
                fn get_or_set_fields(option_fields: &mut Option<message::FieldHashMap>,
                                     version: message_version::MessageVersion) -> message::FieldHashMap {
                    if option_fields.is_none() {
                        let fields = #message_name::fields(version);
                        *option_fields = Some(fields);
                    }

                    option_fields.as_ref().unwrap().clone()
                }

                match version {
                    message_version::MessageVersion::FIX40 => get_or_set_fields(&mut self.cache.fields_fix40,version),
                    message_version::MessageVersion::FIX41 => get_or_set_fields(&mut self.cache.fields_fix41,version),
                    message_version::MessageVersion::FIX42 => get_or_set_fields(&mut self.cache.fields_fix42,version),
                    message_version::MessageVersion::FIX43 => get_or_set_fields(&mut self.cache.fields_fix43,version),
                    message_version::MessageVersion::FIX44 => get_or_set_fields(&mut self.cache.fields_fix44,version),
                    message_version::MessageVersion::FIX50 => get_or_set_fields(&mut self.cache.fields_fix50,version),
                    message_version::MessageVersion::FIX50SP1 => get_or_set_fields(&mut self.cache.fields_fix50sp1,version),
                    message_version::MessageVersion::FIX50SP2 => get_or_set_fields(&mut self.cache.fields_fix50sp2,version),
                }
            }

            fn required_fields(&self,version: message_version::MessageVersion) -> message::FieldHashSet {
                #message_name::required_fields(version)
            }

            fn new_into_box(&self) -> Box<message::BuildMessage + Send> {
                #build_message_name::new_into_box()
            }

            fn build(&self) -> Box<message::Message + Send> {
                Box::new(#message_name::new())
            }
        }


        impl message::MessageBuildable for #message_name {
            fn builder(&self) -> Box<message::BuildMessage + Send> {
                #build_message_name::new_into_box()
            }

            fn builder_func(&self) -> fn() -> Box<message::BuildMessage + Send> {
                #build_message_name::new_into_box
            }
        }
    };

    if is_fixt_message {
        tokens.extend(quote! {
            impl fixt::message::BuildFIXTMessage for #build_message_name {
                fn new_into_box(&self) -> Box<fixt::message::BuildFIXTMessage + Send> {
                    Box::new(#build_message_name::new())
                }

                fn build(&self) -> Box<fixt::message::FIXTMessage + Send> {
                    Box::new(#message_name::new())
                }
            }

            impl fixt::message::FIXTMessageBuildable for #message_name {
                fn builder(&self) -> Box<fixt::message::BuildFIXTMessage + Send> {
                    Box::new(#build_message_name::new())
                }
            }
        });
    }

    tokens.into()
}

#[proc_macro_derive(BuildField,attributes(tag))]
pub fn build_field(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();

    let tag = match extract_attribute_int(&ast,"_tag_gen","tag") {
        Ok(bytes) => bytes,
        Err(ExtractAttributeError::BodyNotStruct) => panic!("#[derive(BuildField)] can only be used with structs"),
        Err(ExtractAttributeError::FieldNotFound) => panic!("#[derive(BuildField)] requires a _tag_gen field to be specified"),
        Err(ExtractAttributeError::AttributeNotFound) => panic!("#[derive(BuildField)] requires the _tag_gen field to have the tag attribute"),
        Err(ExtractAttributeError::AttributeNotNameValue) |
        Err(ExtractAttributeError::AttributeValueWrongType) => panic!("#[derive(BuildField)] tag attribute must be as an unsigned integer like #[tag=1234]"),
    };

    let field_name = ast.ident;
    let bytes = syn::LitByteStr::new(tag.to_string().as_bytes(), Span::call_site());

    let tokens = quote! {
        impl #field_name {
            fn tag_bytes() -> &'static [u8] {
                #bytes
            }

            fn tag() -> field_tag::FieldTag {
                field_tag::FieldTag(#tag)
            }
        }
    };

    tokens.into()
}
