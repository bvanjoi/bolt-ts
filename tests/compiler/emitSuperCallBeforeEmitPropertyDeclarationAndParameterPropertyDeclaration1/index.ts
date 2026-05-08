// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/emitSuperCallBeforeEmitPropertyDeclarationAndParameterPropertyDeclaration1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A {
    blub = 6;
}


class B extends A {
    blah = 2;
    constructor(public x: number) {
        "use strict";
        'someStringForEgngInject';
        super()
    }
}