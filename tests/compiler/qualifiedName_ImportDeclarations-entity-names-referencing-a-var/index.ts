// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/qualifiedName_ImportDeclarations-entity-names-referencing-a-var.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace Alpha {
    export var x = 100;
}

namespace Beta {
    import p = Alpha.x;
}


var x = Alpha.x