use crate::check::TyChecker;

pub fn pprint_reference_ty<'cx>(
    ty: &super::ReferenceTy<'cx>,
    checker: &mut TyChecker<'cx>,
) -> String {
    let args = ty
        .resolved_ty_args
        .iter()
        .map(|ty| ty.to_string(checker))
        .collect::<Vec<_>>()
        .join(", ");
    if ty.target.kind.as_object_tuple().is_some() {
        assert!(!args.is_empty());
        format!("[{args}]",)
    } else if args.is_empty() {
        ty.target.to_string(checker)
    } else {
        format!("{}<{args}>", ty.deep_target().to_string(checker))
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
