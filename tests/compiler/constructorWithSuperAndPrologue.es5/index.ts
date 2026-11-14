// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constructorWithSuperAndPrologue.es5.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

// https://github.com/microsoft/TypeScript/issues/48761
"use strict";

class A {
    public constructor() {
        console.log("A")
    }
}

class B extends A  {
    constructor() {
        "ngInject";
        console.log("B")
        super();
    }
}