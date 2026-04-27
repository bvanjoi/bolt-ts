// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/emitSuperCallBeforeEmitParameterPropertyDeclaration1ES6.ts`, Apache-2.0 License

//@compiler-options: target=es6

class A {
    blub = 6;
}


class B extends A {
    constructor(public x: number) {
        "use strict";
        'someStringForEgngInject';
        super()
    }
}