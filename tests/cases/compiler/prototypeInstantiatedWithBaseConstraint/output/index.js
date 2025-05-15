// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/prototypeInstantiatedWithBaseConstraint.ts`, Apache-2.0 License
//@ run-fail
class C {
  x
}
C.prototype.x.boo;