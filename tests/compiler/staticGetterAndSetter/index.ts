// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/staticGetterAndSetter.ts`, Apache-2.0 License

class Foo {
  static get Foo():number { return 0; }
  static set Foo(n: boolean) {}
}

let a: string = Foo.Foo;
//~^ ERROR: Type 'number' is not assignable to type 'string'.
Foo.Foo = '';
//~^ ERROR: Type 'string' is not assignable to type 'boolean'.
Foo.Foo = 1;
//~^ ERROR: Type 'number' is not assignable to type 'boolean'.
