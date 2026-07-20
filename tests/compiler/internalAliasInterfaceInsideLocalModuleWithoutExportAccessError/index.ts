// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalAliasInterfaceInsideLocalModuleWithoutExportAccessError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export namespace a {
    export interface I {
    }
}

export namespace c {
    import b = a.I;
    export var x: b;
}

var x: c.b;
//~^ ERROR: Namespace 'c' has no exported member 'b'.