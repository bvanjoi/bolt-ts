// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentCompatibilityForConstrainedTypeParameters.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo<T extends { bar: string }>() {
  function bar<S extends T>() {
    var x: S;
    var y: T;
       y = x;
       //~^ ERROR: Variable 'x' is used before being assigned.
    }
}
