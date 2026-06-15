pub trait SignatureDeclaration<'cx>: std::fmt::Debug {
    fn id(&self) -> crate::NodeID;
    fn type_parameters(&self) -> Option<crate::TyParams<'cx>>;
    fn parameters(&self) -> Option<crate::ParamsDecl<'cx>>;
    fn ty(&self) -> Option<&'cx crate::Ty<'cx>>;
    fn fn_flags(&self) -> crate::FnFlags;
}

impl<'cx> SignatureDeclaration<'cx> for crate::CallSigDecl<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn type_parameters(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn parameters(&self) -> Option<crate::ParamsDecl<'cx>> {
        Some(self.params)
    }
    fn ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        self.ty
    }
    fn fn_flags(&self) -> crate::FnFlags {
        self.fn_flags()
    }
}

impl<'cx> SignatureDeclaration<'cx> for crate::CtorSigDecl<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn type_parameters(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn parameters(&self) -> Option<crate::ParamsDecl<'cx>> {
        Some(self.params)
    }
    fn ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        None
    }
    fn fn_flags(&self) -> crate::FnFlags {
        self.fn_flags()
    }
}

impl<'cx> SignatureDeclaration<'cx> for crate::MethodSignature<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn type_parameters(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn parameters(&self) -> Option<crate::ParamsDecl<'cx>> {
        Some(self.params)
    }
    fn ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        self.ty
    }
    fn fn_flags(&self) -> crate::FnFlags {
        self.fn_flags()
    }
}

impl<'cx> SignatureDeclaration<'cx> for crate::IndexSigDecl<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn type_parameters(&self) -> Option<crate::TyParams<'cx>> {
        None
    }
    fn parameters(&self) -> Option<crate::ParamsDecl<'cx>> {
        None
    }
    fn ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        Some(self.ty)
    }
    fn fn_flags(&self) -> crate::FnFlags {
        self.fn_flags()
    }
}

impl<'cx> SignatureDeclaration<'cx> for crate::FnDecl<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn type_parameters(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn parameters(&self) -> Option<crate::ParamsDecl<'cx>> {
        Some(self.params)
    }
    fn ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        self.ty
    }
    fn fn_flags(&self) -> crate::FnFlags {
        self.fn_flags()
    }
}

impl<'cx> SignatureDeclaration<'cx> for crate::ClassMethodElem<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn type_parameters(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn parameters(&self) -> Option<crate::ParamsDecl<'cx>> {
        Some(self.params)
    }
    fn ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        self.ty
    }
    fn fn_flags(&self) -> crate::FnFlags {
        self.fn_flags()
    }
}

impl<'cx> SignatureDeclaration<'cx> for crate::GetterDecl<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn type_parameters(&self) -> Option<crate::TyParams<'cx>> {
        None
    }
    fn parameters(&self) -> Option<crate::ParamsDecl<'cx>> {
        None
    }
    fn ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        self.ty
    }
    fn fn_flags(&self) -> crate::FnFlags {
        self.fn_flags()
    }
}

impl<'cx> SignatureDeclaration<'cx> for crate::SetterDecl<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn type_parameters(&self) -> Option<crate::TyParams<'cx>> {
        None
    }
    fn parameters(&self) -> Option<crate::ParamsDecl<'cx>> {
        Some(self.params)
    }
    fn ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        None
    }
    fn fn_flags(&self) -> crate::FnFlags {
        self.fn_flags()
    }
}

impl<'cx> SignatureDeclaration<'cx> for crate::ClassCtor<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn type_parameters(&self) -> Option<crate::TyParams<'cx>> {
        None
    }
    fn parameters(&self) -> Option<crate::ParamsDecl<'cx>> {
        Some(self.params)
    }
    fn ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        None
    }
    fn fn_flags(&self) -> crate::FnFlags {
        self.fn_flags()
    }
}

impl<'cx> SignatureDeclaration<'cx> for crate::FnExpr<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn type_parameters(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn parameters(&self) -> Option<crate::ParamsDecl<'cx>> {
        Some(self.params)
    }
    fn ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        self.ty
    }
    fn fn_flags(&self) -> crate::FnFlags {
        self.fn_flags()
    }
}

impl<'cx> SignatureDeclaration<'cx> for crate::ObjectMethodMember<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn type_parameters(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn parameters(&self) -> Option<crate::ParamsDecl<'cx>> {
        Some(self.params)
    }
    fn ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        self.ty
    }
    fn fn_flags(&self) -> crate::FnFlags {
        self.fn_flags()
    }
}

impl<'cx> SignatureDeclaration<'cx> for crate::ArrowFnExpr<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn type_parameters(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn parameters(&self) -> Option<crate::ParamsDecl<'cx>> {
        Some(self.params)
    }
    fn ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        self.ty
    }
    fn fn_flags(&self) -> crate::FnFlags {
        self.fn_flags()
    }
}
