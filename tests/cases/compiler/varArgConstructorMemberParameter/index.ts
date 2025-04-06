// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/varArgConstructorMemberParameter.ts`, Apache-2.0 License

class Foo1 {
  constructor (...args: string[]) { }
}

class Foo2 {
  constructor (public args: string[]) { }
}

class Foo3 {
  constructor (public ...args: string[]) { }
  //~^ ERROR: 'public' cannot be declared using a rest parameter.
}
