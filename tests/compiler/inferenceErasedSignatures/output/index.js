// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferenceErasedSignatures.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
class SomeAbstractClass extends SomeBaseClass {
  foo;
  bar;
}
class SomeClass extends SomeAbstractClass {
  baz(context) {
    return `${context}`;
  }
}