use crate::check::TyChecker;

pub fn pprint_reference_ty<'cx>(
    ty: &super::ReferenceTy<'cx>,
    checker: &mut TyChecker<'cx>,
) -> String {
    if let Some(_) = ty.target.kind.as_object_tuple() {
        format!(
            "[{}]",
            ty.resolved_ty_args
                .iter()
                .map(|ty| ty.to_string(checker))
                .collect::<Vec<_>>()
                .join(",")
        )
    } else {
        ty.target.to_string(checker)
    }
}
