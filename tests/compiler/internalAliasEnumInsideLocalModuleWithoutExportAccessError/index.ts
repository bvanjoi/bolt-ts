// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalAliasEnumInsideLocalModuleWithoutExportAccessError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export namespace a {
    export enum weekend {
        Friday,
        Saturday,
        Sunday
    }
}

export namespace c {
    import b = a.weekend;
    export var bVal: b = b.Sunday;
}

var happyFriday = c.b.Friday;
//~^ ERROR: Property 'b' does not exist on type 'typeof c'.
