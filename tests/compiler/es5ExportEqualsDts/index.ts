// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es5ExportEqualsDts.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: declaration

class A {
    foo() {
        var aVal: A.B;
        return aVal;
        //~^ ERROR: Variable 'aVal' is used before being assigned.
    }
}

namespace A {
    export interface B { }
}

export = A