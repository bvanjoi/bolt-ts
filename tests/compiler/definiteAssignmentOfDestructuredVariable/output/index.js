// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/definiteAssignmentOfDestructuredVariable.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class C {
  foo;
  method() {
    var {a, b} = this.foo;
    !(a && b);
    a;
  }
}