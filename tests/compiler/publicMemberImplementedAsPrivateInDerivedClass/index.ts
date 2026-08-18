// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/publicMemberImplementedAsPrivateInDerivedClass.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Qux {
 Bar: number;
}
class Foo implements Qux {
  //~^ ERROR: Class 'Foo' incorrectly implements interface 'Qux'.
 private Bar: number;
 //~^ ERROR: Property 'Bar' has no initializer and is not definitely assigned in the constructor.
}