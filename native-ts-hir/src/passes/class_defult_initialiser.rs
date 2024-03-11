use crate::ast::{Expr, Program, Type};

pub fn run(program: &mut Program) {
    for (_, class) in &mut program.table.classes {
        for (_, desc) in &mut class.properties {
            if desc.initialiser.is_none() {
                desc.initialiser = create_default_initialiser(&desc.ty);
            }
        }
    }
}

pub fn create_default_initialiser(ty: &Type) -> Option<Expr> {
    match ty {
        Type::Any => Some(Expr::Undefined),
        Type::LiteralBigint(i) => Some(Expr::Bigint(*i)),
        Type::LiteralBool(b) => Some(Expr::Bool(*b)),
        Type::LiteralInt(i) => Some(Expr::Int(*i)),
        Type::LiteralNumber(n) => Some(Expr::Number(*n)),
        Type::LiteralString(s) => Some(Expr::String(s.to_string())),
        Type::Null => Some(Expr::Null),
        Type::Undefined => Some(Expr::Undefined),
        Type::Union(u) => {
            if u.contains(&Type::Undefined) {
                Some(Expr::Cast(Box::new(Expr::Undefined), ty.clone()))
            } else {
                None
            }
        }
        _ => None,
    }
}
