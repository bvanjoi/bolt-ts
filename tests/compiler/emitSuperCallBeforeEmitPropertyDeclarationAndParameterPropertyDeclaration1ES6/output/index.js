// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/emitSuperCallBeforeEmitPropertyDeclarationAndParameterPropertyDeclaration1.ts`, Apache-2.0 License
class A {
  blub = 6;
}
class B extends A {
  blah = 2;
  constructor(x) {
    'use strict';
    'someStringForEgngInject';
    super();
    this.x = x
    }
}