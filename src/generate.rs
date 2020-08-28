use std::fmt;
use std::fs::File;
use std::io::{self, Read, Write};
use std::path::Path;

use heck::SnakeCase;
use serde::{Deserialize, Deserializer, de::{self, Visitor}};
use serde_xml_rs::from_str;

use thiserror::Error;

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
struct FixXmlSchema {
    major: u8,
    minor: u8,
    header: FixHeaderSchema,
    messages: FixMessagesSchema,
    components: FixComponentsSchema,
    fields: FixFieldsSchema,
    trailer: FixTrailerSchema,
}

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
struct FixHeaderSchema {
    #[serde(rename = "$value")]
    items: Vec<FixItemSchema>
}

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
struct FixMessagesSchema {
    #[serde(rename = "message")]
    messages: Vec<FixMessageSchema>
}

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
struct FixMessageSchema {
    name: String,
    msgtype: String,
    msgcat: FixMessageCategory,
    #[serde(rename = "$value")]
    items: Vec<FixItemSchema>
}

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
struct FixTrailerSchema {
    #[serde(rename = "$value")]
    items: Vec<FixItemSchema>
}

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
struct FixFieldSchema {
    name: String,
    #[serde(deserialize_with = "from_y_n")]
    required: bool
}

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
struct FixComponentsSchema {
    #[serde(rename = "component")]
    components: Vec<FixComponentDefinition>
}

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
struct FixComponentDefinition {
    name: String,
    #[serde(rename = "$value")]
    items: Vec<FixItemSchema>
}

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
struct FixFieldsSchema {
    #[serde(rename = "field")]
    fields: Vec<FixFieldDefinition>
}

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
struct FixFieldDefinition {
    number: u16,
    name: String,
    #[serde(rename = "type")]
    ty: String,
    #[serde(rename = "value", default = "Vec::new")]
    values: Vec<FixFieldValue>,
    #[serde(rename = "allowOtherValues", default)]
    allow_other_values: bool,
}

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
struct FixFieldValue {
    #[serde(rename = "enum")]
    enumeration: String,
    description: String,
}

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
struct FixComponentSchema {
    name: String,
    #[serde(deserialize_with = "from_y_n")]
    required: bool
}

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
struct FixGroupSchema {
    name: String,
    #[serde(deserialize_with = "from_y_n")]
    required: bool,
    #[serde(rename = "$value")]
    items: Vec<FixItemSchema>
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "lowercase")]
enum FixItemSchema {
    Component(FixComponentSchema),
    Group(FixGroupSchema),
    Field(FixFieldSchema),
}

#[derive(Deserialize, Debug, Eq, PartialEq)]
#[serde(rename_all = "lowercase")]
enum FixMessageCategory {
    Admin,
    App,
}

fn from_y_n<'de, D>(deserializer: D) -> Result<bool, D::Error>
where D: Deserializer<'de>
{
    struct YesNoVisitor;
    impl<'de> Visitor<'de> for YesNoVisitor {
        type Value = bool;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("Y/N")
        }

        fn visit_str<E: de::Error>(self, s: &str) -> Result<bool, E> {
            match s {
                "y" | "Y" => Ok(true),
                "n" | "N" => Ok(false),
                _ => Err(de::Error::invalid_value(de::Unexpected::Str(s), &self))
            }
        }
    }
    deserializer.deserialize_str(YesNoVisitor)
}

#[derive(Error, Debug)]
pub enum GenerateError {
    #[error("failed to write bindings: {0}")]
    Output(#[from] io::Error),
    #[error("failed to read input file: {0}")]
    Input(io::Error),
    #[error("failed to parse xml: {0}")]
    XmlError(#[from] serde_xml_rs::Error),
}

pub fn generate_dictionary<P: AsRef<Path>>(src: P, dest: P) -> Result<(), GenerateError> {
    let mut file_contents = String::new();
    File::open(src)
        .map_err(GenerateError::Input)?
        .read_to_string(&mut file_contents)
        .map_err(GenerateError::Input)?;

    let mut f = File::create(dest)?;

    let schema: FixXmlSchema = from_str(&file_contents)?;

    f.write(
r#"use fix_rs::field::Field;
use fix_rs::field_tag::{self, FieldTag};
use fix_rs::field_type::FieldType;
use fix_rs::fix_version::FIXVersion;
use fix_rs::fix::{self, parse_meta};
use fix_rs::fixt;
use fix_rs::dictionary::field_types::generic::StringFieldType;
use fix_rs::fixt::message::{FIXTMessage, TagValue};
use fix_rs::message::{self, Message, Meta, SetValueError, NOT_REQUIRED, REQUIRED};
use fix_rs::message_version::{self, MessageVersion};
use fix_rs::{define_fields, define_fixt_message, define_dictionary};"#.as_bytes()
    )?;

    writeln!(f)?;

    writeln!(f, "pub mod fields {{")?;
    writeln!(f, "use super::*;")?;
    writeln!(f, "define_fields!(")?;

    for field in &schema.fields.fields {
        writeln!(
            f,
            "    {}: StringFieldType = {},",
            field.name,
            field.number
        )?;
    }

    writeln!(f, ");")?;
    writeln!(f, "}}")?;

    writeln!(f, "use fields::*;")?;

    writeln!(f, "pub mod messages {{")?;
    writeln!(f, "use super::*;")?;

    for message in &schema.messages.messages {
        writeln!(
            f,
            r#"define_fixt_message!({}: {} b"{}" => {{"#,
            message.name,
            if message.msgcat == FixMessageCategory::Admin { "ADMIN" } else { "" },
            message.msgtype
        )?;

        for item in &message.items {
            match item {
                FixItemSchema::Field(field) => {
                    writeln!(
                        f,
                        "    {}, {}: {} [FIX44..],",
                        if field.required { "REQUIRED" } else { "NOT_REQUIRED" },
                        field.name.to_snake_case(),
                        field.name
                    )?;
                },
                _ => {} // FIXME: Groups and Components
            }
        }

        writeln!(f, "}});")?;
    }

    writeln!(f, "}}")?;

    writeln!(f, "use messages::*;")?;

    writeln!(f, "define_dictionary!(")?;

    for message in &schema.messages.messages {
        writeln!(f, "    {},", message.name)?;
    }

    writeln!(f, ");")?;

    Ok(())
}
