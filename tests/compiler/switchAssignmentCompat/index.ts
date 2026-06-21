// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/switchAssignmentCompat.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Foo { }

switch (0) {
    case Foo: break; // Error expected
    //~^ ERROR: Type 'typeof Foo' is not comparable to type '0'.
}