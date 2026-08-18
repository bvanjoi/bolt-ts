// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferTypeParameterConstraints.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
class BaseClass {
  fake() {
    throw new Error('')
  }
}
class Klass extends BaseClass {
  child = true;
}

m.child;