// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/emitSuperCallBeforeEmitPropertyDeclaration1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A {
    blub = 6;
}


class B extends A {

    blub = 12;

    constructor() {
        "use strict";
        'someStringForEgngInject';
        super()
    }
}