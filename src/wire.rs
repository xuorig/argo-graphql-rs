type BlockKey = String;

#[derive(Debug, Clone)]
pub enum Type {
    Record {
        fields: Vec<Field>,
    },
    Nullable(Box<Type>),
    Array(Box<Type>),
    Desc,
    Block {
        of: Box<Type>,
        key: BlockKey,
        dedupe: bool,
    },
    String,
    VarInt,
    Float64,
    Boolean,
}

impl Type {
    pub fn inner(self) -> Type {
        match self {
            Type::Nullable(of) => of.inner(),
            Type::Array(of) => of.inner(),
            Type::Block { of, .. } => of.inner(),
            _ => self,
        }
    }
}

pub(crate) fn nullable(inner: Type) -> Type {
    Type::Nullable(Box::new(inner))
}

pub(crate) fn array(inner: Type) -> Type {
    Type::Array(Box::new(inner))
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub r#type: Type,
    omittable: bool,
}

impl Field {
    pub fn new(name: String, r#type: Type, omittable: bool) -> Self {
        Self {
            name,
            r#type,
            omittable,
        }
    }
}
