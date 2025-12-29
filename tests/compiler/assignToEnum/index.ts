// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/assignToEnum.ts`, Apache-2.0 License

enum A { foo, bar }
A = undefined;  // invalid LHS
//~^ ERROR: Cannot assign to 'A' because it is a enum.
A = A.bar;      // invalid LHS
//~^ ERROR: Cannot assign to 'A' because it is a enum.
A.foo = 1;      // invalid LHS
//~^ ERROR: Cannot assign to 'foo' because it is a read-only property.
A.foo = A.bar;  // invalid LHS
//~^ ERROR: Cannot assign to 'foo' because it is a read-only property.

{
  type Foo = {
	  readonly a: number;
  };
  type StripReadonly<T> = {
    -readonly [KeyType in keyof T]: T[KeyType]
  }
  const ab: StripReadonly<Foo> = {a: 1};
  ab.a = 2;
}