// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parameterPropertyOutsideConstructor.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C {
    foo(public x) {
      //~^ ERROR: A parameter property is only allowed in a constructor implementation.
    }
}