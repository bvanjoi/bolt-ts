// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/assignmentCompatability44.ts`, Apache-2.0 License

class Foo {
  constructor(x: number) {}
}

const foo: { new(): Foo } = Foo;
//~^ ERROR: Type 'typeof Foo' is not assignable to type 'new () => Foo'.
