// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferTypeParameterConstraints.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail
// [1 | 2 | 3 | 4]
// unknown[]
// Repro from #42636
// string
// https://github.com/microsoft/TypeScript/issues/57286#issuecomment-1927920336
class BaseClass {
  fake() {
    throw new Error('')
  }
}
class Klass extends BaseClass {
  child = true;
}

m.child;