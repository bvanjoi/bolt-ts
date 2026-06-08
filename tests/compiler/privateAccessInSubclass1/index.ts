// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/privateAccessInSubclass1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Base {
  private options: any;
}

class D extends Base {
  myMethod() {
    this.options;
    //~^ ERROR: Property 'options' is private and only accessible within class 'Base'.
  }
}