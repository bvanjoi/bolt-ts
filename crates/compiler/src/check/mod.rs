mod assign;
mod check_bin_like;
mod check_call_like;
mod check_class_decl_like;
mod check_deferred;
mod check_expr;
mod check_fn_like_decl;
mod check_fn_like_expr;
mod check_fn_like_symbol;
mod check_interface;
mod check_stmt;
mod check_ty;
mod check_ty_refer_ty_or_import;
mod check_type_related_to;
mod check_var_like;
mod compare_tys;
mod create_ty;
mod cycle_check;
mod elaborate_error;
mod errors;
mod expect;
mod get_base_ty;
mod get_context;
mod get_contextual;
mod get_declared_ty;
mod get_effective_node;
mod get_sig;
mod get_this_ty;
mod get_ty;
mod get_type_from_ty_refer_like;
mod get_type_from_var_like;
mod get_widened_ty;
mod index_info;
mod infer;
mod instantiate;
mod instantiation_ty_map;
mod is_context_sensitive;
mod is_deeply_nested_type;
mod links;
mod node_flags;
mod relation;
mod resolve;
mod resolve_structured_member;
mod type_assignable;
mod utils;

use bolt_ts_atom::{AtomId, AtomMap};

use bolt_ts_utils::fx_hashmap_with_capacity;
use rustc_hash::{FxBuildHasher, FxHashMap};

use self::cycle_check::ResolutionKey;
use self::get_context::{InferenceContextual, TyContextual};
use self::get_contextual::ContextFlags;
use self::infer::InferenceContext;
use self::infer::{InferenceFlags, InferencePriority};
use self::instantiation_ty_map::InstantiationTyMap;
use self::links::NodeLinks;
pub use self::links::SymbolLinks;
use self::links::{SigLinks, TyLinks};
use self::node_flags::NodeFlags;
pub use self::resolve::ExpectedArgsCount;

use crate::ast::{pprint_ident, BinOp};
use crate::bind::{self, GlobalSymbols, Symbol, SymbolFlags, SymbolID, SymbolName};
use crate::parser::{AssignmentKind, Parser};
use crate::ty::has_type_facts;
use crate::ty::TYPEOF_NE_FACTS;
use crate::ty::{
    CheckFlags, ElementFlags, ObjectFlags, Sig, SigFlags, SigID, TyID, TyKind, TypeFacts, TypeFlags,
};
use crate::{ast, ecma_rules, ensure_sufficient_stack, keyword, ty};

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub struct Ternary: u8 {
        const FALSE = 0x0;
        const UNKNOWN = 0x1;
        const MAYBE = 0x3;
        const TRUE = u8::MAX;
    }
}

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub struct CheckMode: u8 {
        const CONTEXTUAL                = 1 << 0;
        const INFERENTIAL               = 1 << 1;
        const SKIP_CONTEXT_SENSITIVE    = 1 << 2;
        const SKIP_GENERIC_FUNCTIONS    = 1 << 3;
        const IS_FOR_SIGNATURE_HELP     = 1 << 4;
        const REST_BINDING_ELEMENT      = 1 << 5;
        const TYPE_ONLY                 = 1 << 6;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct F64Represent {
    inner: u64,
}

impl F64Represent {
    fn new(val: f64) -> Self {
        Self {
            inner: unsafe { std::mem::transmute::<f64, u64>(val) },
        }
    }
}

impl From<f64> for F64Represent {
    fn from(val: f64) -> Self {
        F64Represent::new(val)
    }
}

impl From<usize> for F64Represent {
    fn from(val: usize) -> Self {
        F64Represent::new(val as f64)
    }
}

impl From<F64Represent> for f64 {
    fn from(val: F64Represent) -> Self {
        f64::from_bits(val.inner)
    }
}

bolt_ts_utils::index!(InferenceContextId);

pub struct TyChecker<'cx> {
    pub atoms: &'cx AtomMap<'cx>,
    pub diags: Vec<bolt_ts_errors::Diag>,
    arena: &'cx bumpalo::Bump,
    tys: Vec<&'cx ty::Ty<'cx>>,
    sigs: Vec<&'cx Sig<'cx>>,
    num_lit_tys: FxHashMap<F64Represent, TyID>,
    string_lit_tys: FxHashMap<AtomId, TyID>,
    intrinsic_tys: FxHashMap<AtomId, &'cx ty::Ty<'cx>>,
    type_name: FxHashMap<TyID, String>,
    tuple_tys: FxHashMap<u64, &'cx ty::Ty<'cx>>,

    check_mode: Option<CheckMode>,
    inferences: Vec<InferenceContext<'cx>>,
    inference_contextual: Vec<InferenceContextual>,
    type_contextual: Vec<TyContextual<'cx>>,
    deferred_nodes: Vec<indexmap::IndexSet<ast::NodeID, FxBuildHasher>>,
    // === links ===
    symbol_links: FxHashMap<SymbolID, SymbolLinks<'cx>>,
    node_links: FxHashMap<ast::NodeID, NodeLinks<'cx>>,
    sig_links: FxHashMap<SigID, SigLinks<'cx>>,
    ty_links: FxHashMap<TyID, TyLinks<'cx>>,
    instantiation_ty_map: InstantiationTyMap<'cx>,
    // === ast ===
    pub p: &'cx Parser<'cx>,
    // === global ===
    boolean_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    string_or_number_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    string_number_symbol_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    any_array_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    typeof_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    unknown_sig: std::cell::OnceCell<&'cx Sig<'cx>>,
    any_fn_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    circular_constraint_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    no_constraint_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    empty_generic_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    empty_object_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_object_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_fn_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_callable_fn_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_newable_fn_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_array_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_readonly_array_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_number_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_string_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_boolean_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    // === resolver ===
    pub binder: &'cx mut bind::Binder<'cx>,
    global_symbols: &'cx GlobalSymbols,

    resolution_start: i32,
    resolution_tys: thin_vec::ThinVec<ResolutionKey>,
    resolution_res: thin_vec::ThinVec<bool>,
}

macro_rules! intrinsic_type {
    ($(($name: ident, $atom_id: expr, $ty_flags: expr)),* $(,)?) => {
        impl<'cx> TyChecker<'cx> {
            $(pub fn $name(&self) -> &'cx ty::Ty<'cx> {
                self.intrinsic_tys[&$atom_id]
            })*
        }
        static INTRINSIC_TYPES: &[(TyKind, AtomId)] = &[
            $(($ty_flags, $atom_id),)*
        ];
    };
}

intrinsic_type!(
    (any_ty, keyword::IDENT_ANY, TyKind::Any),
    (unknown_ty, keyword::IDENT_UNKNOWN, TyKind::Unknown),
    (never_ty, keyword::IDENT_NEVER, TyKind::Never),
    (error_ty, keyword::IDENT_ERROR, TyKind::Any),
    (void_ty, keyword::KW_VOID, TyKind::Void),
    (undefined_ty, keyword::IDENT_UNDEFINED, TyKind::Undefined),
    (null_ty, keyword::KW_NULL, TyKind::Null),
    (true_ty, keyword::KW_TRUE, TyKind::TrueLit),
    (false_ty, keyword::KW_FALSE, TyKind::FalseLit),
    (number_ty, keyword::IDENT_NUMBER, TyKind::Number),
    (string_ty, keyword::IDENT_STRING, TyKind::String),
    (
        non_primitive_ty,
        keyword::IDENT_OBJECT,
        TyKind::NonPrimitive
    ),
);

enum PropName {
    String(AtomId),
    Num(f64),
}

impl<'cx> TyChecker<'cx> {
    pub fn new(
        ty_arena: &'cx bumpalo::Bump,
        p: &'cx Parser<'cx>,
        atoms: &'cx AtomMap,
        binder: &'cx mut bind::Binder<'cx>,
        global_symbols: &'cx GlobalSymbols,
    ) -> Self {
        assert_eq!(ty_arena.allocated_bytes(), 0);
        let mut this = Self {
            intrinsic_tys: fx_hashmap_with_capacity(1024),
            atoms,
            p,
            tys: Vec::with_capacity(p.module_count() * 1024),
            sigs: Vec::with_capacity(p.module_count() * 256),
            arena: ty_arena,
            diags: Vec::with_capacity(p.module_count() * 32),

            num_lit_tys: fx_hashmap_with_capacity(1024 * 8),
            string_lit_tys: fx_hashmap_with_capacity(1024 * 8),
            instantiation_ty_map: InstantiationTyMap::new(1024 * 16),

            any_fn_ty: Default::default(),
            circular_constraint_ty: Default::default(),
            no_constraint_ty: Default::default(),
            empty_generic_ty: Default::default(),
            empty_object_ty: Default::default(),
            boolean_ty: Default::default(),
            typeof_ty: Default::default(),
            string_or_number_ty: Default::default(),
            string_number_symbol_ty: Default::default(),
            any_array_ty: Default::default(),
            global_number_ty: Default::default(),
            global_string_ty: Default::default(),
            global_boolean_ty: Default::default(),
            global_array_ty: Default::default(),
            global_readonly_array_ty: Default::default(),
            global_fn_ty: Default::default(),
            global_callable_fn_ty: Default::default(),
            global_newable_fn_ty: Default::default(),
            global_object_ty: Default::default(),

            unknown_sig: Default::default(),

            type_name: fx_hashmap_with_capacity(1024 * 8),

            symbol_links: fx_hashmap_with_capacity(p.module_count() * 1024),
            node_links: fx_hashmap_with_capacity(p.module_count() * 1024),
            sig_links: fx_hashmap_with_capacity(p.module_count() * 1024),
            ty_links: fx_hashmap_with_capacity(p.module_count() * 1024),
            tuple_tys: fx_hashmap_with_capacity(p.module_count() * 1024),

            resolution_tys: thin_vec::ThinVec::with_capacity(128),
            resolution_res: thin_vec::ThinVec::with_capacity(128),
            resolution_start: 0,

            binder,
            global_symbols,
            inferences: Vec::with_capacity(p.module_count() * 1024),
            inference_contextual: Vec::with_capacity(256),
            type_contextual: Vec::with_capacity(256),
            check_mode: None,
            deferred_nodes: vec![
                indexmap::IndexSet::with_capacity_and_hasher(64, FxBuildHasher);
                p.module_count()
            ],
        };
        for (kind, ty_name) in INTRINSIC_TYPES {
            let ty = if *ty_name == keyword::IDENT_ERROR {
                this.any_ty()
            } else {
                this.new_ty(*kind)
            };
            let prev = this.intrinsic_tys.insert(*ty_name, ty);
            assert!(prev.is_none());
        }
        let boolean_ty = this.create_union_type(
            vec![this.true_ty(), this.false_ty()],
            ty::UnionReduction::Lit,
        );
        this.type_name.insert(boolean_ty.id, "boolean".to_string());
        this.boolean_ty.set(boolean_ty).unwrap();

        let string_or_number_ty = this.create_union_type(
            vec![
                this.string_ty(),
                this.number_ty(), /* TODO: symbol_ty */
            ],
            ty::UnionReduction::Lit,
        );
        this.string_or_number_ty.set(string_or_number_ty).unwrap();

        let string_number_symbol_ty = this.create_union_type(
            vec![
                this.string_ty(),
                this.number_ty(), /* TODO: symbol_ty */
            ],
            ty::UnionReduction::Lit,
        );
        this.string_number_symbol_ty
            .set(string_number_symbol_ty)
            .unwrap();

        let global_number_ty =
            this.get_global_type(SymbolName::Normal(keyword::IDENT_NUMBER_CLASS));
        this.global_number_ty.set(global_number_ty).unwrap();

        let global_boolean_ty =
            this.get_global_type(SymbolName::Normal(keyword::IDENT_BOOLEAN_CLASS));
        this.global_boolean_ty.set(global_boolean_ty).unwrap();

        let global_array_ty = this.get_global_type(SymbolName::Normal(keyword::IDENT_ARRAY_CLASS));
        this.global_array_ty.set(global_array_ty).unwrap();

        let global_readonly_array_ty =
            this.get_global_type(SymbolName::Normal(keyword::IDENT_ARRAY_CLASS));
        this.global_readonly_array_ty
            .set(global_readonly_array_ty)
            .unwrap();

        let global_string_ty =
            this.get_global_type(SymbolName::Normal(keyword::IDENT_STRING_CLASS));
        this.global_string_ty.set(global_string_ty).unwrap();

        let typeof_ty = {
            let tys = TYPEOF_NE_FACTS
                .iter()
                .map(|(key, _)| this.get_string_literal_type(*key))
                .collect();
            this.create_union_type(tys, ty::UnionReduction::Lit)
        };
        this.typeof_ty.set(typeof_ty).unwrap();

        let any_array_ty = this.create_array_ty(this.any_ty());
        this.any_array_ty.set(any_array_ty).unwrap();

        let any_fn_ty = this.create_anonymous_ty_with_resolved(
            None,
            ObjectFlags::NON_INFERRABLE_TYPE,
            this.alloc(Default::default()),
            Default::default(),
            Default::default(),
            Default::default(),
        );
        this.any_fn_ty.set(any_fn_ty).unwrap();

        let no_constraint_ty = this.create_anonymous_ty_with_resolved(
            None,
            Default::default(),
            this.alloc(Default::default()),
            Default::default(),
            Default::default(),
            Default::default(),
        );

        this.no_constraint_ty.set(no_constraint_ty).unwrap();

        let circular_constraint_ty = this.create_anonymous_ty_with_resolved(
            None,
            Default::default(),
            this.alloc(Default::default()),
            Default::default(),
            Default::default(),
            Default::default(),
        );
        this.circular_constraint_ty
            .set(circular_constraint_ty)
            .unwrap();

        let empty_generic_ty = this.create_anonymous_ty_with_resolved(
            None,
            Default::default(),
            this.alloc(Default::default()),
            Default::default(),
            Default::default(),
            Default::default(),
        );

        this.empty_generic_ty.set(empty_generic_ty).unwrap();

        let empty_object_ty = this.create_anonymous_ty_with_resolved(
            None,
            Default::default(),
            this.alloc(Default::default()),
            Default::default(),
            Default::default(),
            Default::default(),
        );

        this.empty_object_ty.set(empty_object_ty).unwrap();

        let global_object_ty =
            this.get_global_type(SymbolName::Normal(keyword::IDENT_OBJECT_CLASS));
        this.global_object_ty.set(global_object_ty).unwrap();

        let global_fn_ty = this.get_global_type(SymbolName::Normal(keyword::IDENT_FUNCTION_CLASS));
        this.global_fn_ty.set(global_fn_ty).unwrap();

        let global_callable_fn_ty =
            this.get_global_type(SymbolName::Normal(keyword::IDENT_CALLABLE_FUNCTION_CLASS));
        this.global_callable_fn_ty
            .set(global_callable_fn_ty)
            .unwrap();

        let global_newable_fn_ty =
            this.get_global_type(SymbolName::Normal(keyword::IDENT_NEWABLE_FUNCTION_CLASS));
        this.global_newable_fn_ty.set(global_newable_fn_ty).unwrap();

        let unknown_sig = this.new_sig(Sig {
            flags: SigFlags::empty(),
            ty_params: None,
            params: &[],
            min_args_count: 0,
            ret: None,
            node_id: None,
            target: None,
            mapper: None,
            id: SigID::dummy(),
            class_decl: None,
        });
        this.unknown_sig.set(unknown_sig).unwrap();

        this
    }

    fn check_flags(&self, symbol: SymbolID) -> CheckFlags {
        self.binder.get_check_flags(symbol)
    }

    pub fn print_ty<'a>(&'a mut self, ty: &'cx ty::Ty<'cx>) -> &'a str {
        let type_name = (!self.type_name.contains_key(&ty.id)).then(|| ty.to_string(self));
        self.type_name
            .entry(ty.id)
            .or_insert_with(|| type_name.unwrap())
    }

    pub fn unknown_sig(&self) -> &'cx Sig<'cx> {
        self.unknown_sig.get().unwrap()
    }

    fn alloc<T>(&self, t: T) -> &'cx T {
        self.arena.alloc(t)
    }

    pub fn check_program(&mut self, program: &'cx ast::Program<'cx>) {
        for stmt in program.stmts {
            self.check_stmt(stmt);
        }
    }

    fn is_applicable_index_ty(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
    ) -> bool {
        self.is_type_assignable_to(source, target) != Ternary::FALSE
            || (target == self.string_ty()
                && self.is_type_assignable_to(source, self.number_ty()) != Ternary::FALSE)
    }

    fn get_base_constraint_of_ty(&self, ty: &'cx ty::Ty<'cx>) -> Option<&'cx ty::Ty<'cx>> {
        if ty.kind.is_param() {
            self.param_ty_constraint(ty)
        } else {
            Some(ty)
        }
    }

    fn get_apparent_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        let ty = if ty.kind.is_instantiable() {
            self.get_base_constraint_of_ty(ty)
                .unwrap_or(self.undefined_ty())
        } else {
            ty
        };
        if ty.kind.is_number_like() {
            self.global_number_ty()
        } else if ty.kind.is_string_like() {
            self.global_string_ty()
        } else if ty.kind.is_boolean_like() {
            self.global_boolean_ty()
        } else {
            ty
        }
    }

    fn get_reduced_apparent_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        self.get_apparent_ty(ty)
    }

    fn get_applicable_index_infos(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        prop_name_ty: &'cx ty::Ty<'cx>,
    ) -> Vec<&'cx ty::IndexInfo<'cx>> {
        self.index_infos_of_ty(ty)
            .iter()
            .filter_map(|info| {
                if self.is_applicable_index_ty(prop_name_ty, info.key_ty) {
                    Some(*info)
                } else {
                    None
                }
            })
            .collect()
    }

    fn check_index_constraint_for_prop(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        prop: SymbolID,
        prop_name_ty: &'cx ty::Ty<'cx>,
        prop_ty: &'cx ty::Ty<'cx>,
    ) {
        for index_info in self.get_applicable_index_infos(ty, prop_name_ty) {
            if self.is_type_assignable_to(prop_ty, index_info.val_ty) == Ternary::FALSE {
                let prop_decl = prop.decl(self.binder);
                let prop_node = self.p.node(prop_decl);
                let prop_name = match prop_node {
                    ast::Node::ClassPropElem(prop) => prop.name,
                    ast::Node::PropSignature(prop) => prop.name,
                    _ => unreachable!(),
                };
                let prop_name = match prop_name.kind {
                    ast::PropNameKind::Ident(ident) => self.atoms.get(ident.name).to_string(),
                    ast::PropNameKind::NumLit(num) => num.val.to_string(),
                    ast::PropNameKind::StringLit { raw, .. } => {
                        format!("\"{}\"", self.atoms.get(raw.val))
                    }
                };
                let error = errors::PropertyAOfTypeBIsNotAssignableToCIndexTypeD {
                    span: prop_node.span(),
                    prop: prop_name,
                    ty_b: self.print_ty(prop_ty).to_string(),
                    ty_c: self.print_ty(index_info.key_ty).to_string(),
                    index_ty_d: self.print_ty(index_info.val_ty).to_string(),
                };
                self.push_error(Box::new(error));
                return;
            }
        }

        if let Some(_) = ty.kind.as_object_interface() {
            let base_tys = self
                .get_ty_links(ty.id)
                .expect_structured_members()
                .base_tys;
            for base_ty in base_tys {
                self.check_index_constraint_for_prop(base_ty, prop, prop_name_ty, prop_ty);
            }
        } else {
            // unreachable!("{:#?}", ty)
        }
    }

    fn get_lit_ty_from_prop_name(
        &mut self,
        prop_name: &'cx ast::PropName<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        match prop_name.kind {
            ast::PropNameKind::Ident(ident) => self.get_string_literal_type(ident.name),
            ast::PropNameKind::NumLit(num) => self.get_number_literal_type(num.val),
            ast::PropNameKind::StringLit { key, .. } => self.get_string_literal_type(key),
        }
    }

    fn get_lit_ty_from_prop(&mut self, prop: SymbolID) -> &'cx ty::Ty<'cx> {
        let symbol = self.binder.symbol(prop);
        let Some(prop) = prop.opt_decl(self.binder) else {
            return if let Some(name) = symbol.name.as_atom() {
                self.get_string_literal_type(name)
            } else {
                self.never_ty()
            };
        };
        let name = match self.p.node(prop) {
            ast::Node::ClassPropElem(prop) => prop.name,
            ast::Node::ObjectPropMember(prop) => prop.name,
            ast::Node::PropSignature(prop) => prop.name,
            ast::Node::ClassMethodElem(prop) => prop.name,
            ast::Node::MethodSignature(prop) => prop.name,
            ast::Node::ParamDecl(_) => return self.string_ty(),
            _ => unreachable!("prop: {:#?}", self.p.node(prop)),
        };
        self.get_lit_ty_from_prop_name(name)
    }

    fn check_index_constraints(&mut self, ty: &'cx ty::Ty<'cx>, symbol: SymbolID) {
        self.resolve_structured_type_members(ty);
        for prop in self.properties_of_object_type(ty) {
            let prop_ty = self.get_type_of_symbol(*prop);
            let prop_name_ty = self.get_lit_ty_from_prop(*prop);
            self.check_index_constraint_for_prop(ty, *prop, prop_name_ty, prop_ty);
        }
    }

    fn get_containing_fn_or_class_static_block(&self, node: ast::NodeID) -> Option<ast::NodeID> {
        self.p.find_ancestor(node, |node| {
            if node.is_fn_like_or_class_static_block_decl() {
                Some(true)
            } else {
                None
            }
        })
    }

    fn instantiate_ty_with_single_generic_call_sig(
        &mut self,
        expr: &'cx ast::Expr<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let Some(check_mode) = self.check_mode else {
            return ty;
        };
        if !check_mode.intersects(CheckMode::INFERENTIAL | CheckMode::SKIP_GENERIC_FUNCTIONS) {
            return ty;
        }
        let Some((sig, kind)) = self
            .get_single_sig(ty, ty::SigKind::Call, true)
            .map(|sig| (sig, ty::SigKind::Call))
            .or_else(|| {
                self.get_single_sig(ty, ty::SigKind::Constructor, true)
                    .map(|sig| (sig, ty::SigKind::Constructor))
            })
        else {
            return ty;
        };
        let Some(ty_params) = sig.ty_params else {
            return ty;
        };
        let Some(contextual_ty) =
            self.get_apparent_ty_of_contextual_ty(expr.id(), Some(ContextFlags::NoConstraints))
        else {
            return ty;
        };
        let Some(contextual_sig) = self.get_single_sig(contextual_ty, kind, false) else {
            return ty;
        };
        if contextual_sig.ty_params.is_some() {
            return ty;
        }
        if check_mode.intersects(CheckMode::SKIP_GENERIC_FUNCTIONS) {
            self.skip_generic_fn(expr.id(), check_mode);
            return self.any_fn_ty();
        }

        let context = self.get_inference_context(expr.id()).unwrap();
        let inference = context.inference.unwrap();
        let ret_sig = self
            .get_inference_sig(inference)
            .map(|sig| self.get_ret_ty_of_sig(sig))
            .and_then(|ret_ty| self.get_single_call_or_ctor_sig(ret_ty));
        if let Some(ret_sig) = ret_sig {
            if ret_sig.ty_params.is_none()
                && !self
                    .inference_infos(inference)
                    .iter()
                    .all(|info| info.has_inference_candidates())
            {
                todo!()
            }
        }

        let sig = self.instantiate_sig_in_context_of(sig, contextual_sig, Some(inference));
        let outer_ty_params = self
            .inference_contextual
            .iter()
            .filter_map(|inference| {
                inference.inference.map(|inference| {
                    self.inference_infos(inference)
                        .iter()
                        .map(|info| info.ty_param)
                        .collect::<Vec<_>>()
                })
            })
            .flatten()
            .collect::<Vec<_>>();
        self.get_or_create_ty_from_sig(sig, Some(self.alloc(outer_ty_params)))
    }

    fn get_or_create_ty_from_sig(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        outer_ty_params: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        //TODO:cache
        let is_constructor = sig.node_id.map_or(true, |node_id| {
            use ast::Node::*;
            matches!(self.p.node(node_id), ClassCtor(_) | CtorSigDecl(_))
        });
        if let Some(node_id) = sig.node_id {
            // decls in symbol
        }
        let symbol = self.binder.create_transient_symbol(
            SymbolName::Fn,
            SymbolFlags::FUNCTION,
            None,
            SymbolLinks::default(),
        );
        let outer_ty_params: Option<ty::Tys<'cx>> = if let Some(outer_ty_params) = outer_ty_params {
            Some(outer_ty_params)
        } else if let Some(id) = sig.node_id {
            if let Some(ty_params) = self.get_outer_ty_params(id, true) {
                Some(self.alloc(ty_params))
            } else {
                None
            }
        } else {
            None
        };
        let ty = self.create_single_sig_ty(ty::SingleSigTy {
            symbol,
            target: None,
            mapper: None,
            outer_ty_params,
        });
        let prev = self.ty_links.insert(
            ty.id,
            TyLinks::default().with_structured_members(self.alloc(ty::StructuredMembers {
                members: self.alloc(Default::default()),
                base_tys: &[],
                base_ctor_ty: None,
                call_sigs: if !is_constructor {
                    self.alloc([sig])
                } else {
                    &[]
                },
                ctor_sigs: if is_constructor {
                    self.alloc([sig])
                } else {
                    &[]
                },
                index_infos: &[],
                props: &[],
            })),
        );
        assert!(prev.is_none());
        ty
    }

    fn instantiate_sig_in_context_of(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        contextual_sig: &'cx ty::Sig<'cx>,
        inference_context: Option<InferenceContextId>,
    ) -> &'cx ty::Sig<'cx> {
        let context = {
            let ty_params = self.get_ty_params_for_mapper(sig);
            self.create_inference_context(ty_params, Some(sig), InferenceFlags::empty())
        };
        let rest_ty = contextual_sig.get_rest_ty(self);
        let mut mapper = None;
        if let Some(inference_context) = inference_context {
            if let Some(rest_ty) = rest_ty {
                if rest_ty.kind.is_param() {
                    mapper = Some(self.create_inference_non_fixing_mapper(inference_context))
                }
            } else {
                mapper = Some(self.create_inference_fixing_mapper(inference_context))
            }
        };
        let source_sig = if let Some(mapper) = mapper {
            self.instantiate_sig(contextual_sig, mapper, false)
        } else {
            contextual_sig
        };
        self.apply_to_param_tys(source_sig, sig, |this, source, target| {
            this.infer_tys(context, source, target, None, false)
        });
        if inference_context.is_none() {
            self.apply_to_ret_ty(contextual_sig, sig, |this, source, target| {
                this.infer_tys(
                    context,
                    source,
                    target,
                    Some(InferencePriority::RETURN_TYPE),
                    false,
                );
            });
        }

        let ty_args = self.get_inferred_tys(context);
        self.get_sig_instantiation(sig, Some(ty_args), false, None)
    }

    fn skip_generic_fn(&mut self, node: ast::NodeID, check_mode: CheckMode) {
        if check_mode.intersects(CheckMode::INFERENTIAL) {
            let context = self.get_inference_context(node).unwrap();
            let inference = context.inference.unwrap();
            self.config_inference_flags(inference, |flags| {
                *flags |= InferenceFlags::SKIPPED_GENERIC_FUNCTION;
            });
        }
    }

    fn try_get_this_ty_at(
        &mut self,
        expr: &'cx ast::ThisExpr,
        include_global_this: bool,
        container_id: Option<ast::NodeID>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let container_id =
            container_id.unwrap_or_else(|| self.p.get_this_container(expr.id, false, false));
        let container = self.p.node(container_id);
        if container.is_fn_like() {
            let this_ty = self
                .get_this_ty_of_decl(container_id)
                .or_else(|| self.get_contextual_this_param_ty(container_id));

            if let Some(this_ty) = this_ty {
                return Some(this_ty);
            }
        }

        if let Some(parent) = self.p.parent(container_id) {
            let p = self.p.node(parent);
            if p.is_class_like() {
                let symbol = self.get_symbol_of_decl(parent);
                let this_ty = if container.is_static() {
                    self.get_type_of_symbol(symbol)
                } else {
                    let ty = self
                        .get_declared_ty_of_symbol(symbol)
                        .kind
                        .expect_object_reference();
                    let ty = ty.target.kind.expect_object_interface();
                    ty.this_ty.unwrap()
                };
                return Some(this_ty);
            }
        }
        None
    }

    fn is_for_in_variable_for_numeric_prop_names(&self, expr: &'cx ast::Expr<'cx>) -> bool {
        let id = expr.id();
        let e = self.p.skip_parens(id);
        let e = self.p.node(e);
        if let Some(ident) = e.as_ident() {
            let symbol = self.resolve_symbol_by_ident(ident);
            if self
                .binder
                .symbol(symbol)
                .flags
                .intersects(SymbolFlags::VARIABLE)
            {
                let child = id;
                while let Some(parent) = self.p.parent(child) {
                    let node = self.p.node(parent);
                    if let Some(_) = node.as_for_in_stmt() {
                        todo!()
                    }
                }
            }
        }
        false
    }

    fn type_has_static_prop(
        &mut self,
        prop_node: &'cx ast::Ident,
        containing_ty: &'cx ty::Ty<'cx>,
    ) -> bool {
        let Some(symbol) = containing_ty.symbol() else {
            return false;
        };
        let ty = self.get_type_of_symbol(symbol);
        let name = SymbolName::Ele(prop_node.name);
        let Some(prop) = self.get_prop_of_ty(ty, name) else {
            return false;
        };
        let decl = prop.decl(self.binder);
        self.p.node(decl).is_static()
    }

    fn report_non_existent_prop(
        &mut self,
        prop_node: &'cx ast::Ident,
        containing_ty: &'cx ty::Ty<'cx>,
    ) {
        if self
            .get_ty_links(containing_ty.id)
            .get_non_existent_prop_checked()
            .unwrap_or_default()
        {
            return;
        }
        self.get_mut_ty_links(containing_ty.id)
            .set_non_existent_prop_checked(true);

        let missing_prop = pprint_ident(prop_node, self.atoms);

        if self.type_has_static_prop(prop_node, containing_ty) {
            let mut error = errors::PropertyXDoesNotExistOnTypeY {
                span: prop_node.span,
                prop: missing_prop,
                ty: self.print_ty(containing_ty).to_string(),
                related: vec![],
            };
            error.related.push(errors::PropertyXDoesNotExistOnTypeYHelperKind::DidYourMeanToAccessTheStaticMemberInstead(
                errors::DidYourMeanToAccessTheStaticMemberInstead {
                    span: prop_node.span,
                    class_name: self.print_ty(containing_ty).to_string(),
                    prop_name: pprint_ident(prop_node, self.atoms),
                }),
            );
            self.push_error(Box::new(error));
        } else if containing_ty != self.undefined_ty() {
            // FIXME: remove `containing_ty != self.undefined_ty()`
            let error = errors::PropertyXDoesNotExistOnTypeY {
                span: prop_node.span,
                prop: missing_prop,
                ty: self.print_ty(containing_ty).to_string(),
                related: vec![],
            };
            self.push_error(Box::new(error));
        }
    }

    pub(super) fn _get_prop_of_ty(
        &mut self,
        left: &'cx ty::Ty<'cx>,
        prop: &'cx ast::Ident,
    ) -> &'cx ty::Ty<'cx> {
        let is_any_like = left.kind.is_any();

        if is_any_like {
            return self.error_ty();
        }

        let Some(prop) = self.get_prop_of_ty(left, SymbolName::Ele(prop.name)) else {
            self.report_non_existent_prop(prop, left);
            return self.error_ty();
        };

        let ty = self.get_type_of_symbol(prop);
        ty
    }

    fn check_prop_access_expr(&mut self, node: &'cx ast::PropAccessExpr<'cx>) -> &'cx ty::Ty<'cx> {
        let left = self.check_expr(node.expr);
        self._get_prop_of_ty(left, node.name)
    }

    fn param_ty_constraint(&self, ty: &'cx ty::Ty<'cx>) -> Option<&'cx ty::Ty<'cx>> {
        let param = ty.kind.expect_param();
        if param.is_this_ty {
            // assert!(param.constraint.is_none());
            Some(self.tys[ty.id.as_usize() + 2])
        } else {
            // TODO:
            None
        }
    }

    fn param_ty_mapper(&self, ty: &'cx ty::Ty<'cx>) -> Option<&'cx ty::TyMapper<'cx>> {
        let param = ty.kind.expect_param();
        param
            .target
            .is_some()
            .then(|| self.expect_ty_links(ty.id).expect_param_ty_mapper())
    }

    fn check_destructing_assign(
        &mut self,
        node: &'cx ast::AssignExpr<'cx>,
        right_ty: &'cx ty::Ty<'cx>,
        right_is_this: bool,
    ) -> &'cx ty::Ty<'cx> {
        if let ast::ExprKind::ArrayLit(array) = node.left.kind {
            if array.elems.is_empty() && right_ty.kind.is_array(self) {
                return right_ty;
            }
        }
        let left_ty = self.check_expr(node.left);
        self.check_binary_like_expr(node, left_ty, right_ty)
    }

    fn check_object_prop_member(
        &mut self,
        member: &'cx ast::ObjectPropMember<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let ty = self.check_expr_for_mutable_location(member.value);
        ty
    }

    fn check_array_lit(&mut self, lit: &'cx ast::ArrayLit, force_tuple: bool) -> &'cx ty::Ty<'cx> {
        let mut element_types = Vec::with_capacity(lit.elems.len());
        let mut element_flags = Vec::with_capacity(lit.elems.len());

        for elem in lit.elems.iter() {
            element_types.push(self.check_expr_for_mutable_location(elem));
            element_flags.push(ElementFlags::REQUIRED);
        }

        if force_tuple {
            let element_types: ty::Tys<'cx> = self.alloc(element_types);
            let element_flags: &'cx [ElementFlags] = self.alloc(element_flags);
            let tuple_ty = self.create_tuple_ty(element_types, Some(element_flags), false);
            self.create_array_literal_ty(tuple_ty)
        } else {
            let ty = if element_types.is_empty() {
                self.never_ty()
            } else {
                self.create_union_type(element_types, ty::UnionReduction::Subtype)
            };
            let array_ty = self.create_array_ty(ty);
            self.create_array_literal_ty(array_ty)
        }
    }

    fn create_array_literal_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if !ty.get_object_flags().intersects(ObjectFlags::REFERENCE) {
            ty
        } else if let Some(literal_ty) = self.get_ty_links(ty.id).get_literal_ty() {
            literal_ty
        } else {
            let object_flags = ty.get_object_flags()
                | ObjectFlags::ARRAY_LITERAL
                | ObjectFlags::CONTAINS_OBJECT_OR_ARRAY_LITERAL;
            let (target, resolved_ty_args) = if let Some(tuple) = ty.kind.as_object_tuple() {
                (ty, tuple.resolved_ty_args)
            } else {
                let ty = ty.kind.expect_object_reference();
                (ty.target, ty.resolved_ty_args)
            };
            let literal_ty = self.create_reference_ty(target, resolved_ty_args, object_flags);
            self.get_mut_ty_links(ty.id).set_literal_ty(literal_ty);
            literal_ty
        }
    }

    fn is_used_in_fn_or_instance_prop(&self, used: &'cx ast::Ident, decl: ast::NodeID) -> bool {
        assert!(used.id.module() == decl.module());
        self.p
            .find_ancestor(used.id, |current| {
                let current_id = current.id();
                if current_id == decl {
                    return Some(false);
                } else if current.is_fn_like() {
                    return Some(true);
                } else if current.is_class_static_block_decl() {
                    return Some(self.p.node(decl).span().lo < used.span.lo);
                }

                let Some(parent_id) = self.p.parent(current_id) else {
                    return None;
                };
                let parent_node = self.p.node(parent_id);
                let Some(prop_decl) = parent_node.as_class_prop_ele() else {
                    return None;
                };

                let init_of_prop = prop_decl
                    .init
                    .map(|init| init.id() == current_id)
                    .unwrap_or_default();
                if init_of_prop {
                    if parent_node.is_static() {
                        let n = self.p.node(decl);
                        if n.is_class_method_ele() {
                            return Some(true);
                        } else if let Some(prop_decl) = n.as_class_prop_ele() {
                            if let Some(usage_class) = self.p.get_containing_class(used.id) {
                                if let Some(decl_class) = self.p.get_containing_class(decl) {
                                    if usage_class == decl_class {
                                        let prop_name = prop_decl.name;
                                        todo!()
                                    }
                                }
                            }
                        }
                    }
                } else {
                    let n = self.p.node(decl);
                    let is_decl_instance_prop = n.is_class_prop_ele() && !n.is_static();
                    if !is_decl_instance_prop {
                        return Some(true);
                    } else if let Some(usage_class) = self.p.get_containing_class(used.id) {
                        if let Some(decl_class) = self.p.get_containing_class(decl) {
                            if usage_class == decl_class {
                                return Some(true);
                            }
                        }
                    }
                }
                None
            })
            .is_some()
    }

    fn is_block_scoped_name_declared_before_use(
        &self,
        decl: ast::NodeID,
        used: &'cx ast::Ident,
    ) -> bool {
        let used_span = used.span;
        let decl_span = self.p.node(decl).span();
        let decl_pos = decl_span.lo;
        if decl_pos < used_span.lo {
            return true;
        }

        let decl_container = self.p.get_enclosing_blockscope_container(decl);

        if self.is_used_in_fn_or_instance_prop(used, decl) {
            return true;
        }

        false
    }

    fn check_resolved_block_scoped_var(&mut self, ident: &'cx ast::Ident, id: SymbolID) {
        let symbol = self.binder.symbol(id).expect_class();
        let decl = symbol.decl;
        if !self.is_block_scoped_name_declared_before_use(decl, ident) {
            let decl_span = match self.p.node(decl) {
                ast::Node::ClassDecl(class) => class.name.span,
                _ => unreachable!(),
            };
            let kind = errors::DeclKind::Class;
            let name = self.atoms.get(ident.name).to_string();
            let error = errors::CannotUsedBeforeItsDeclaration {
                span: ident.span,
                kind,
                name: name.to_string(),
                related: [errors::DefinedHere {
                    span: decl_span,
                    kind,
                    name,
                }],
            };
            self.push_error(Box::new(error));
        }
    }

    fn check_ident(&mut self, ident: &'cx ast::Ident) -> &'cx ty::Ty<'cx> {
        if ident.name == keyword::IDENT_UNDEFINED {
            return self.undefined_ty();
        }

        let symbol = self.resolve_symbol_by_ident(ident);

        if self.binder.symbol(symbol).flags == SymbolFlags::CLASS {
            self.check_resolved_block_scoped_var(ident, symbol);
        }

        let ty = self.get_type_of_symbol(symbol);
        let assignment_kind = self.p.get_assignment_kind(ident.id);
        if assignment_kind != AssignmentKind::None && symbol != Symbol::ERR {
            let symbol = self.binder.symbol(symbol);
            if !symbol.is_variable() {
                let error = errors::CannotAssignToNameBecauseItIsATy {
                    span: ident.span,
                    name: self.atoms.get(ident.name).to_string(),
                    ty: symbol.as_str().to_string(),
                };
                self.push_error(Box::new(error));
                return self.error_ty();
            }
        }

        ty
    }

    fn check_bin_expr(&mut self, node: &'cx ast::BinExpr) -> &'cx ty::Ty<'cx> {
        let l = self.check_expr(node.left);
        let r = ensure_sufficient_stack(|| self.check_expr(node.right));
        self.check_bin_like_expr(node, node.op, node.left, l, node.right, r)
    }

    fn check_non_null_type(&mut self, expr: &'cx ast::Expr) -> &'cx ty::Ty<'cx> {
        if matches!(expr.kind, ast::ExprKind::NullLit(_)) {
            let error = errors::TheValueCannotBeUsedHere {
                span: expr.span(),
                value: "null".to_string(),
            };
            self.push_error(Box::new(error));
            self.null_ty()
        } else if matches!(expr.kind, ast::ExprKind::Ident(ast::Ident { name, .. }) if *name == keyword::IDENT_UNDEFINED)
        {
            let error = errors::TheValueCannotBeUsedHere {
                span: expr.span(),
                value: "undefined".to_string(),
            };
            self.push_error(Box::new(error));
            self.null_ty()
        } else {
            self.null_ty()
        }
    }

    fn check_binary_like_expr_for_add(
        &mut self,
        left_ty: &'cx ty::Ty<'cx>,
        right_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if self.is_type_assignable_to_kind(
            left_ty,
            |ty| ty.kind.is_number_like(),
            TypeFlags::NUMBER_LIKE,
            true,
        ) & self.is_type_assignable_to_kind(
            right_ty,
            |ty| ty.kind.is_number_like(),
            TypeFlags::NUMBER_LIKE,
            true,
        ) != Ternary::FALSE
        {
            Some(self.number_ty())
        } else if self.is_type_assignable_to_kind(
            left_ty,
            |ty| ty.kind.is_string_like(),
            TypeFlags::STRING_LIKE,
            true,
        ) | self.is_type_assignable_to_kind(
            right_ty,
            |ty| ty.kind.is_string_like(),
            TypeFlags::STRING_LIKE,
            true,
        ) != Ternary::FALSE
        {
            Some(self.string_ty())
        } else if left_ty == self.any_ty() || right_ty == self.any_ty() {
            Some(self.any_ty())
        } else {
            None
        }
    }

    fn check_bin_like_expr(
        &mut self,
        node: &'cx ast::BinExpr,
        op: BinOp,
        left: &'cx ast::Expr,
        left_ty: &'cx ty::Ty<'cx>,
        right: &'cx ast::Expr,
        right_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        use ast::BinOpKind::*;
        match op.kind {
            Add => {
                let ty = match self.check_binary_like_expr_for_add(left_ty, right_ty) {
                    Some(ty) => ty,
                    None => {
                        let error = errors::OperatorCannotBeAppliedToTy1AndTy2 {
                            op: op.kind.to_string(),
                            ty1: left_ty.to_string(self),
                            ty2: right_ty.to_string(self),
                            span: node.span,
                        };
                        self.push_error(Box::new(error));
                        self.any_ty()
                    }
                };
                ty
            }
            Sub => self.number_ty(),
            Mul => self.undefined_ty(),
            Div => todo!(),
            Pipe => {
                let left = self.check_non_null_type(left);
                let right = self.check_non_null_type(right);
                self.number_ty()
            }
            LogicalAnd => {
                if has_type_facts(left_ty, TypeFacts::TRUTHY) {
                    left_ty
                } else {
                    right_ty
                }
            }
            PipePipe => {
                if has_type_facts(left_ty, TypeFacts::FALSE_FACTS) {
                    right_ty
                } else {
                    left_ty
                }
            }
            EqEq => self.boolean_ty(),
            EqEqEq => self.boolean_ty(),
            Less => self.boolean_ty(),
            LessEq => self.boolean_ty(),
            Shl => todo!(),
            Great => todo!(),
            GreatEq => todo!(),
            Shr => todo!(),
            UShr => todo!(),
            BitAnd => todo!(),
            Instanceof => self.boolean_ty(),
            In => self.check_in_expr(left, left_ty, right, right_ty),
            Satisfies => todo!(),
        }
    }

    fn check_in_expr(
        &mut self,
        left: &'cx ast::Expr,
        left_ty: &'cx ty::Ty<'cx>,
        right: &'cx ast::Expr,
        right_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        self.check_type_assignable_to(left_ty, self.string_number_symbol_ty(), Some(left.id()));
        if self.check_type_assignable_to(right_ty, self.non_primitive_ty(), Some(right.id()))
            == Ternary::FALSE
        {
            let right_ty = self.get_widened_literal_ty(right_ty);
            let error = ecma_rules::TheRightValueOfTheInOperatorMustBeAnObjectButGotTy {
                span: right.span(),
                ty: right_ty.to_string(self),
            };
            self.push_error(Box::new(error));
        }
        self.boolean_ty()
    }

    fn push_error(&mut self, error: crate::Diag) {
        self.diags.push(bolt_ts_errors::Diag { inner: error })
    }

    fn check_ty_param(&mut self, ty_param: &'cx ast::TyParam<'cx>) {
        if let Some(constraint) = ty_param.constraint {
            self.check_ty(constraint);
        }
    }

    fn check_indexed_access_index_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        n: &'cx ast::IndexedAccessTy<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let Some(indexed_access_ty) = ty.kind.as_indexed_access() else {
            return ty;
        };
        let object_index_ty =
            self.get_index_ty(indexed_access_ty.object_ty, ty::IndexFlags::empty());
        let has_number_index_info = self
            .get_index_info_of_ty(indexed_access_ty.object_ty, self.number_ty())
            .is_some();
        if self.every_type(indexed_access_ty.index_ty, |this, t| {
            this.is_type_assignable_to(t, object_index_ty) != Ternary::FALSE
                || (has_number_index_info && this.is_applicable_index_ty(t, this.number_ty()))
        }) {
            return ty;
        }

        if indexed_access_ty.object_ty.kind.is_generic_object() {
            // TODO:
        }

        let error = errors::TypeCannotBeUsedToIndexType {
            span: n.span,
            ty: self.print_ty(indexed_access_ty.index_ty).to_string(),
            index_ty: self.print_ty(indexed_access_ty.object_ty).to_string(),
        };
        self.push_error(Box::new(error));
        self.error_ty()
    }

    fn check_ty_params(&mut self, ty_params: ast::TyParams<'cx>) {
        // let mut seen_default = false;
        for ty_param in ty_params {
            self.check_ty_param(ty_param)
        }
    }

    pub fn check_and_aggregate_ret_expr_tys(
        &mut self,
        f: ast::NodeID,
        body: &'cx ast::BlockStmt<'cx>,
    ) -> Option<Vec<&'cx ty::Ty<'cx>>> {
        let flags = self.p.node(f).fn_flags();
        let mut has_ret_with_no_expr = false;
        let has_ret_of_ty_never = false;

        fn for_each_ret_stmt<'cx, T>(
            id: ast::NodeID,
            checker: &mut TyChecker<'cx>,
            f: impl Fn(&mut TyChecker<'cx>, &'cx ast::RetStmt<'cx>) -> T + Copy,
            has_ret_with_no_expr: &mut bool,
        ) -> Vec<T> {
            fn t<'cx, T>(
                id: ast::NodeID,
                checker: &mut TyChecker<'cx>,
                f: impl Fn(&mut TyChecker<'cx>, &'cx ast::RetStmt<'cx>) -> T + Copy,
                v: &mut Vec<T>,
                has_ret_with_no_expr: &mut bool,
            ) {
                let node = checker.p.node(id);
                if let Some(ret) = node.as_ret_stmt() {
                    if let Some(ret_expr) = ret.expr {
                        // todo: handle return of type never
                    } else {
                        *has_ret_with_no_expr = true;
                    }
                    v.push(f(checker, ret))
                } else if let Some(b) = node.as_block_stmt() {
                    for stmt in b.stmts {
                        t(stmt.id(), checker, f, v, has_ret_with_no_expr);
                    }
                } else if let Some(node) = node.as_if_stmt() {
                    t(node.then.id(), checker, f, v, has_ret_with_no_expr);
                    if let Some(else_then) = node.else_then {
                        t(else_then.id(), checker, f, v, has_ret_with_no_expr);
                    }
                } else {
                    return;
                }
            }

            let mut v = Vec::with_capacity(8);
            t(id, checker, f, &mut v, has_ret_with_no_expr);
            v
        }

        let tys = for_each_ret_stmt(
            body.id,
            self,
            |this, ret| {
                let Some(expr) = ret.expr else {
                    return this.undefined_ty();
                };
                let old = if let Some(check_mode) = this.check_mode {
                    let old = this.check_mode;
                    this.check_mode = Some(check_mode & !CheckMode::SKIP_GENERIC_FUNCTIONS);
                    old
                } else {
                    None
                };
                let ty = this.check_expr_with_cache(expr);
                this.check_mode = old;
                ty
            },
            &mut has_ret_with_no_expr,
        );

        if tys.is_empty() && !has_ret_with_no_expr && has_ret_of_ty_never {
            None
        } else {
            Some(tys)
        }
    }

    fn is_array_or_tuple(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.kind.is_array(self) || ty.kind.is_tuple()
    }

    fn is_known_prop(&mut self, target: &'cx ty::Ty<'cx>, name: SymbolName) -> bool {
        if let Some(_) = target.kind.as_object() {
            if self.get_prop_of_ty(target, name).is_some() {
                return true;
            }
        }

        false
    }

    fn ty_has_call_or_ctor_sigs(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        !self
            .get_signatures_of_type(ty, ty::SigKind::Call)
            .is_empty()
            || !self
                .get_signatures_of_type(ty, ty::SigKind::Constructor)
                .is_empty()
    }

    fn is_object_ty_with_inferable_index(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        if ty.kind.is_intersection() {
            // TODO:
            false
        } else if let Some(symbol) = ty.symbol() {
            let flags = self.binder.symbol(symbol).flags;
            flags.intersects(
                SymbolFlags::OBJECT_LITERAL
                    | SymbolFlags::TYPE_LITERAL
                    | SymbolFlags::ENUM
                    | SymbolFlags::VALUE_MODULE,
            ) && !flags.intersects(SymbolFlags::CLASS)
                && !self.ty_has_call_or_ctor_sigs(ty)
        } else {
            false
        }
    }

    fn is_empty_anonymous_object_ty(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.kind.as_object_anonymous().is_some_and(|a| {
            if let Some(symbol) = ty.symbol() {
                let s = self.binder.symbol(symbol);
                s.flags.intersects(SymbolFlags::TYPE_LITERAL)
                    && s.expect_ty_lit().members.is_empty()
            } else if let Some(ty_link) = self.ty_links.get(&ty.id) {
                if let Some(t) = ty_link.get_structured_members() {
                    ty != self.any_fn_ty()
                        && t.props.is_empty()
                        && t.call_sigs.is_empty()
                        && t.ctor_sigs.is_empty()
                        && t.index_infos.is_empty()
                } else {
                    false
                }
            } else {
                false
            }
        })
    }

    fn get_simplified_ty_or_constraint(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let simplified = self.get_simplified_ty(ty);
        if simplified != ty {
            Some(simplified)
        } else {
            self.get_constraint_of_ty(ty)
        }
    }

    fn get_simplified_ty(&self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if let Some(_) = ty.kind.as_indexed_access() {
            // TODO: handle indexed_access
            ty
        } else if let Some(_) = ty.kind.as_cond_ty() {
            // TODO: handle indexed_cond
            ty
        } else {
            ty
        }
    }

    fn get_key_prop_name(&self, union_ty: &'cx ty::UnionTy<'cx>) -> Option<SymbolName> {
        None
    }

    fn get_ty_of_prop_of_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        name: SymbolName,
    ) -> Option<&'cx ty::Ty<'cx>> {
        self.get_prop_of_ty(ty, name)
            .map(|prop| self.get_type_of_symbol(prop))
    }

    fn get_matching_union_constituent_for_ty(
        &mut self,
        union_ty: &'cx ty::UnionTy<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let key_prop_name = self.get_key_prop_name(union_ty);
        let prop_ty = key_prop_name.and_then(|name| self.get_ty_of_prop_of_ty(ty, name));
        // TODO: prop_ty.and_then(|ty| self.)
        None
    }
}

macro_rules! global_ty {
    ($($name: ident),* $(,)?) => {
        impl<'cx> TyChecker<'cx> {
            $(
                #[inline(always)]
                pub fn $name(&self) -> &'cx ty::Ty<'cx> {
                    self.$name.get().unwrap()
                }
            )*
        }
    };
}

global_ty!(
    boolean_ty,
    string_or_number_ty,
    string_number_symbol_ty,
    any_array_ty,
    any_fn_ty,
    circular_constraint_ty,
    empty_generic_ty,
    empty_object_ty,
    no_constraint_ty,
    typeof_ty,
    global_number_ty,
    global_string_ty,
    global_boolean_ty,
    global_array_ty,
    global_readonly_array_ty,
    global_fn_ty,
    global_callable_fn_ty,
    global_newable_fn_ty,
    global_object_ty
);
