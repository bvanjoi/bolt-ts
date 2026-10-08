// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/emitSuperCallBeforeEmitParameterPropertyDeclaration1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class A {
  blub = 6;
}
class B extends A {
  constructor(x) {
    'use strict';
    'someStringForEgngInject';
    super();
    this.x = x
    }
}