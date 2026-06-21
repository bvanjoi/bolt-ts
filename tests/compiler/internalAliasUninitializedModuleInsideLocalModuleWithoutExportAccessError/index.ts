// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalAliasUninitializedModuleInsideLocalModuleWithoutExportAccessError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: module=commonjs

export namespace a {
    export namespace b {
        export interface I {
            foo();
        }
    }
}

export namespace c {
    import b = a.b;
    export declare var x: b.I;
    x.foo();
}


export var z: c.b.I;
//~^ ERROR: Namespace 'c' has no exported member 'b'.