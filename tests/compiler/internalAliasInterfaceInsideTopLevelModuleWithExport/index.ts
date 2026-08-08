// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalAliasInterfaceInsideTopLevelModuleWithExport.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: declaration


export namespace a {
    export interface I {
    }
}

export import b = a.I;
export var x: b;
