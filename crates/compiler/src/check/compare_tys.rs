use super::ty;

trait CompareTypes<'cx> {
    fn compare_tys(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        report_error: bool,
    ) -> bool;
}
