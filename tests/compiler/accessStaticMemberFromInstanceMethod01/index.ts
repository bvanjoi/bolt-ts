// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/accessStaticMemberFromInstanceMethod01.ts`, Apache-2.0 License

class C {
  foo: string;
  //~^ ERROR: Property 'foo' has no initializer and is not definitely assigned in the constructor.

  static bar() {
      let k = foo;
      //~^ ERROR: Cannot find name 'foo'.
  }
}
