// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalAliasInitializedModuleInsideLocalModuleWithoutExportAccessError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export namespace a {
    export namespace b {
        export class c {
        }
    }
}

export namespace c {
    import b = a.b;
    export var x: b.c = new b.c();
}

export var d = new c.b.c();
//~^ ERROR: Property 'b' does not exist on type 'typeof c'.