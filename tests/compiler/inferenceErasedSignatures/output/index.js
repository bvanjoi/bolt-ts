// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferenceErasedSignatures.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail
class SomeAbstractClass extends SomeBaseClass {
  foo;
  bar;
}
class SomeClass extends SomeAbstractClass {
  baz(context) {
    return `${context}`;
  }
}// number
// string
// boolean
// Repro from #37163
// This declaration shouldn't do anything...
// Structural expansion of InheritedType
// number
