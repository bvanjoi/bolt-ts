// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/internalAliasFunctionInsideLocalModuleWithoutExportAccessError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export namespace a {
    export function foo(x: number) {
        return x;
    }
}

export namespace c {
    import b = a.foo;
    var bVal = b(10);
    export var bVal2 = b;
}
var d = c.b(11);
//~^ ERROR: Property 'b' does not exist on type 'typeof c'.