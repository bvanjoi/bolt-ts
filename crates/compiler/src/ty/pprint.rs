use super::ElementFlags;
use crate::check::TyChecker;

pub fn pprint_tuple_ty<'cx>(
    ty: &'cx super::TupleTy<'cx>,
    ty_args: super::Tys<'cx>,
    checker: &mut TyChecker<'cx>,
) -> String {
    format!(
        "{}[{}]",
        if ty.readonly { "readonly " } else { "" },
        ty_args
            .iter()
            .enumerate()
            .map(|(i, t)| {
                let flags = ty.element_flags[i];
                let t = t.to_string(checker);
                if flags.intersects(ElementFlags::REQUIRED) {
                    t
                } else if flags.intersects(ElementFlags::OPTIONAL) {
                    format!("{}?", t)
                } else if flags.intersects(ElementFlags::REST) {
                    format!("...{}[]", t)
                } else if flags.intersects(ElementFlags::VARIADIC) {
                    format!("...{}", t)
                } else {
                    unreachable!()
                }
            })
            .collect::<Vec<_>>()
            .join(", ")
    )
}

pub fn pprint_reference_ty<'cx>(ty: &'cx super::Ty<'cx>, checker: &mut TyChecker<'cx>) -> String {
    let refer = ty.kind.expect_object_reference();
    let ty_args = checker.get_ty_arguments(ty);
    if ty_args.is_empty() {
        return refer.target.to_string(checker);
    } else if let Some(ty) = ty.as_tuple() {
        return pprint_tuple_ty(ty, ty_args, checker);
    }
    let args = ty_args
        .iter()
        .map(|ty| ty.to_string(checker))
        .collect::<Vec<_>>()
        .join(", ");
    if let Some(i) = refer.interface_target() {
        format!("{}<{args}>", i.to_string(checker))
    } else {
        unreachable!()
    }
}

// pub fn pprint_mapper<'cx>(mapper: &super::TyMapper<'cx>, checker: &mut TyChecker<'cx>) -> String {
//     use super::TyMapper::*;
//     match mapper {
//         Simple(mapper) => {
//             format!(
//                 "{} -> {}",
//                 mapper.source.to_string(checker),
//                 mapper.target.to_string(checker),
//             )
//         }
//         Array(mapper) => {
//             format!(
//                 "[{}] -> [{}]",
//                 mapper
//                     .sources
//                     .iter()
//                     .map(|ty| ty.to_string(checker))
//                     .collect::<Vec<_>>()
//                     .join(","),
//                 mapper
//                     .targets
//                     .as_ref()
//                     .map(|targets| {
//                         targets
//                             .iter()
//                             .map(|ty| ty.to_string(checker))
//                             .collect::<Vec<_>>()
//                             .join(",")
//                     })
//                     .unwrap_or_else(|| "any".to_string()),
//             )
//         }
//         // Fn(_) => todo!(),
//         Composite(mapper) => {
//             format!(
//                 "{} -> {}",
//                 pprint_mapper(mapper.mapper1, checker),
//                 pprint_mapper(mapper.mapper2, checker),
//             )
//         }
//         Merged(mapper) => {
//             format!(
//                 "{} -> {}",
//                 pprint_mapper(mapper.mapper1, checker),
//                 pprint_mapper(mapper.mapper2, checker),
//             )
//         }
//     }
// }
