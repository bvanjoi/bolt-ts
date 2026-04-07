// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeParameterWithInvalidConstraintType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
class A<T extends T> {
  //~^ ERROR: Type parameter 'T' has a circular constraint.
    foo() {
        var x!: T;
        var a = x.foo();
  //~^ ERROR: Property 'foo' does not exist on type 'T'.
        var b = new x(123);
        //~^ ERROR: This expression is not constructable.
        var c = x[1];
        var d = x();
        //~^ ERROR: This expression is not callable.
    }
}