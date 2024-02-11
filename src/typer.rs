use std::collections::{HashMap, HashSet};

use apollo_compiler::{
    executable::{DirectiveList, Field, Fragment, Selection, SelectionSet},
    schema::{ExtendedType, Name, Type, Value},
    validation::Valid,
    ExecutableDocument, Node, Schema,
};

use indexmap::IndexMap;
use thiserror::Error;

use crate::wire::{array, nullable};

use super::wire;

const SKIP_DIRECTIVE: &str = "skip";
const INCLUDE_DIRECTIVE: &str = "include";

#[derive(Error, Debug)]
pub enum TyperError {
    #[error("Operation `{0}` not found in document")]
    OperationNotFound(String),
    #[error("Fragment `{0}` not found in document")]
    MissingFragment(String),
    #[error("No root type in schema for operation type `{0}`")]
    RootTypeNotFound(String),
    #[error("Wire types do not support `{0}` types")]
    UnhandledWireType(&'static str),
}

pub struct Typer {
    schema: Valid<Schema>,
}

impl Typer {
    pub fn wire_type<O: AsRef<str>>(
        &self,
        executable_document: Valid<ExecutableDocument>,
        operation_name: Option<O>,
    ) -> Result<wire::Type, TyperError> {
        let operation = executable_document
            .get_operation(operation_name.as_ref().map(|o| o.as_ref()))
            .map_err(|_| {
                let name = operation_name
                    .as_ref()
                    .map(|o| o.as_ref().to_string())
                    .unwrap_or_else(|| String::from("None"));
                TyperError::OperationNotFound(name)
            })?;

        let root_type = self
            .schema
            .root_operation(operation.operation_type)
            .ok_or_else(|| TyperError::RootTypeNotFound(operation.operation_type.to_string()))?;

        let data_record = self.collect_field_wire_types(
            &Type::Named(root_type.clone()),
            &operation.selection_set,
            &executable_document.fragments,
        )?;

        let fields = vec![
            wire::Field::new("data".into(), data_record, false),
            wire::Field::new(
                "errors".into(),
                wire::Type::Array(Box::new(wire::Type::Desc)),
                true,
            ),
        ];

        Ok(wire::Type::Record { fields })
    }

    fn collect_field_wire_types(
        &self,
        type_definition: &Type,
        selection_set: &SelectionSet,
        fragments: &IndexMap<Name, Node<Fragment>>,
    ) -> Result<wire::Type, TyperError> {
        let mut record_fields = Vec::new();

        let mut collector = HashMap::new();
        let mut visited_fragments = HashSet::new();
        self.collect_fields_static(
            selection_set,
            fragments,
            &mut collector,
            &mut visited_fragments,
            None,
        )?;

        for (name, fields) in collector {
            for selected_field in fields {
                let type_condition = match selected_field.selected_by {
                    Selection::Field(_) => None,
                    Selection::FragmentSpread(f) => Some(
                        fragments
                            .get(&f.fragment_name)
                            .expect("Fragment not found")
                            .type_condition(),
                    ),
                    Selection::InlineFragment(f) => f.type_condition.as_ref(),
                };

                let field_has_variable_if_directives =
                    self.has_variable_if_directives(&selected_field.field.directives);
                let selection_has_variable_if_directives = match selected_field.selected_by {
                    Selection::Field(f) => self.has_variable_if_directives(&f.directives),
                    Selection::FragmentSpread(f) => self.has_variable_if_directives(&f.directives),
                    Selection::InlineFragment(f) => self.has_variable_if_directives(&f.directives),
                };

                let type_condition_match = match type_condition {
                    Some(type_condition) => type_condition == type_definition.inner_named_type(),
                    None => true,
                };

                let omittable = !type_condition_match
                    || field_has_variable_if_directives
                    || selection_has_variable_if_directives;

                let is_leaf = selected_field.field.selection_set.selections.is_empty();

                let f = selected_field.field;

                if is_leaf {
                    let wire_type = self.type_to_wire_type(f.ty())?;
                    record_fields.push(wire::Field::new(name.to_string(), wire_type, omittable))
                } else {
                    let wrapped = self.type_to_wire_type(f.ty())?;
                    let typ = self.collect_field_wire_types(f.ty(), &f.selection_set, fragments)?;
                    let typ = wrap(wrapped, typ);
                    record_fields.push(wire::Field::new(name.to_string(), typ, omittable))
                }
            }
        }

        Ok(group_overlapping(&mut record_fields))
    }

    fn collect_fields_static<'a>(
        &self,
        selection_set: &'a SelectionSet,
        fragments: &'a IndexMap<Name, Node<Fragment>>,
        collector: &mut HashMap<&'a Name, Vec<SelectedFieldNode<'a>>>,
        visited_fragments: &mut HashSet<Name>,
        selected_by: Option<&'a Selection>,
    ) -> Result<(), TyperError> {
        for selection in &selection_set.selections {
            match selection {
                Selection::Field(field) => {
                    if self.should_always_skip(&field.directives) {
                        continue;
                    }

                    let selected_by = selected_by.unwrap_or(selection);

                    collector
                        .entry(field.response_key())
                        .or_default()
                        .push(SelectedFieldNode { selected_by, field });
                }
                Selection::FragmentSpread(f) => {
                    if self.should_always_skip(&f.directives) {
                        continue;
                    }

                    if visited_fragments.contains(&f.fragment_name) {
                        continue;
                    }

                    let fragment = fragments
                        .get(&f.fragment_name)
                        .ok_or_else(|| TyperError::MissingFragment(f.fragment_name.to_string()))?;

                    visited_fragments.insert(f.fragment_name.clone());

                    self.collect_fields_static(
                        &fragment.selection_set,
                        fragments,
                        collector,
                        visited_fragments,
                        Some(selection),
                    )?;
                }
                Selection::InlineFragment(f) => {
                    if self.should_always_skip(&f.directives) {
                        continue;
                    }

                    self.collect_fields_static(
                        &f.selection_set,
                        fragments,
                        collector,
                        visited_fragments,
                        Some(selection),
                    )?;
                }
            }
        }

        Ok(())
    }

    fn should_always_skip(&self, directives: &DirectiveList) -> bool {
        if let Some(include) = directives.get(INCLUDE_DIRECTIVE) {
            if let Some(if_arg) = include.argument_by_name("if") {
                if matches!(if_arg.as_ref(), Value::Boolean(false)) {
                    return true;
                }
            }
        }

        if let Some(skip) = directives.get(SKIP_DIRECTIVE) {
            if let Some(if_arg) = skip.argument_by_name("if") {
                if matches!(if_arg.as_ref(), Value::Boolean(true)) {
                    return true;
                }
            }
        }

        false
    }

    fn has_variable_if_directives(&self, directives: &DirectiveList) -> bool {
        if let Some(include) = directives.get(INCLUDE_DIRECTIVE) {
            if let Some(if_arg) = include.argument_by_name("if") {
                if matches!(if_arg.as_ref(), Value::Variable(_)) {
                    return true;
                }
            }
        }

        if let Some(skip) = directives.get(SKIP_DIRECTIVE) {
            if let Some(if_arg) = skip.argument_by_name("if") {
                if matches!(if_arg.as_ref(), Value::Variable(_)) {
                    return true;
                }
            }
        }

        false
    }

    fn type_to_wire_type(&self, type_definition: &Type) -> Result<wire::Type, TyperError> {
        let wire_type = match type_definition {
            Type::Named(typ) => {
                // TODO: proper error
                let typ = self
                    .schema
                    .types
                    .get(typ)
                    .ok_or(TyperError::RootTypeNotFound("".into()))?;
                let name = typ.name().to_string();

                match typ {
                    ExtendedType::Scalar(scalar) => {
                        match scalar.name.as_str() {
                            "String" => wire::nullable(wire::Type::Block {
                                of: Box::new(wire::Type::String),
                                key: name,
                                dedupe: true,
                            }),
                            "ID" => wire::nullable(wire::Type::Block {
                                of: Box::new(wire::Type::String),
                                key: name,
                                dedupe: true,
                            }),
                            "Int" => wire::nullable(wire::Type::Block {
                                of: Box::new(wire::Type::VarInt),
                                key: name,
                                dedupe: false,
                            }),
                            "Float" => wire::nullable(wire::Type::Block {
                                of: Box::new(wire::Type::Float64),
                                key: name,
                                dedupe: false,
                            }),
                            "Boolean" => wire::nullable(wire::Type::Boolean),
                            // Custom scalar
                            // Needs argo directive
                            _ => {
                                todo!()
                            }
                        }
                    }
                    ExtendedType::Enum(_) => wire::nullable(wire::Type::Block {
                        of: Box::new(wire::Type::String),
                        key: name,
                        dedupe: true,
                    }),
                    ExtendedType::Object(_) => {
                        wire::nullable(wire::Type::Record { fields: vec![] })
                    }
                    ExtendedType::Interface(_) => {
                        wire::nullable(wire::Type::Record { fields: vec![] })
                    }
                    ExtendedType::Union(_) => wire::nullable(wire::Type::Record { fields: vec![] }),
                    ExtendedType::InputObject(_) => {
                        return Err(TyperError::UnhandledWireType("Input"))
                    }
                }
            }
            Type::NonNullNamed(inner) => self.type_to_wire_type(&Type::Named(inner.clone()))?,
            Type::List(inner) => {
                let inner = self.type_to_wire_type(inner)?;
                wire::nullable(wire::Type::Array(Box::new(inner)))
            }
            Type::NonNullList(inner) => self.type_to_wire_type(inner)?,
        };

        Ok(wire_type)
    }
}

struct SelectedFieldNode<'a> {
    selected_by: &'a Selection,
    field: &'a Field,
}

fn wrap(wrapped: wire::Type, typ: wire::Type) -> wire::Type {
    match wrapped {
        wire::Type::Record { .. } => typ,
        wire::Type::Nullable(of) => wrap(*of, nullable(typ)),
        wire::Type::Array(of) => wrap(*of, array(typ)),
        wire::Type::Block { of, key, dedupe } => wrap(
            *of,
            wire::Type::Block {
                of: Box::new(typ),
                key,
                dedupe,
            },
        ),
        _ => {
            panic!("Tried to unwrap an impossible type")
        }
    }
}

fn group_overlapping(fields: &mut Vec<wire::Field>) -> wire::Type {
    println!("FIELDS {:#?}", fields);

    let mut merged = Vec::new();

    let mut groups: HashMap<String, Vec<wire::Field>> = HashMap::new();

    for field in fields.drain(..) {
        groups.entry(field.name.clone()).or_default().push(field);
    }

    let mut should_recurse = false;

    for (name, mut fields) in groups {
        if fields.len() == 1 {
            let field = fields.pop().unwrap();
            merged.push(field);
        } else {
            should_recurse = true;

            for field in fields {
                let inner_wire_type = field.r#type.inner();

                if let wire::Type::Record { fields } = inner_wire_type {
                    merged.extend(fields)
                } else {
                    // No Good
                    // TODO
                    panic!("This should be a type with a selection set");
                }
            }
        }

        let wire_type = merged.first().unwrap().r#type.clone();

        if should_recurse {
            let merged_type = group_overlapping(&mut merged);
            let t = wrap(wire_type, merged_type);
            merged.push(wire::Field::new(name, t, false))
        }
    }

    wire::Type::Record { fields: merged }
}

#[cfg(test)]
mod tests {
    use apollo_compiler::{ExecutableDocument, Schema};

    use super::Typer;

    #[test]
    fn test() {
        let schema = Schema::parse_and_validate(
            "type Query { movies: [Movie] } type Movie { id: ID, title: String }",
            "test-schema",
        )
        .unwrap();

        let document = ExecutableDocument::parse_and_validate(
            &schema,
            "query { movies { id title } }",
            "query",
        )
        .unwrap();

        let typer = Typer { schema };

        let wire_type = typer.wire_type(document, None::<&str>).unwrap();

        println!("WIRE TYPE: {:#?}", wire_type);
    }
}
