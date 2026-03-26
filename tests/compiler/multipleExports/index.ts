// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/multipleExports.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: module=commonjs

export namespace M {
    export var v = 0;
    export let x;
}

const x = 0;
export namespace M {
    v;
    export {x};
    //~^ ERROR: Export declaration conflicts with exported declaration of 'x'.
    //~| ERROR: Export declarations are not permitted in a namespace.
}

export namespace J {
    export {a}
    //~^ ERROR: Export declarations are not permitted in a namespace.
    var a = 0;
}