// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalAliasVarInsideLocalModuleWithoutExportAccessError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export namespace a {
    export var x = 10;
}

export namespace c {
    import b = a.x;
    export var bVal = b;
}

export var z = c.b;
//~^ ERROR: Property 'b' does not exist on type 'typeof c'.