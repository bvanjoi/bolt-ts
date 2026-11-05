// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/signatureInstantiationWithRecursiveConstraints.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

class Foo {
  myFunc<T extends Foo>(arg: T) {}
}

class Bar {
  myFunc<T extends Bar>(arg: T) {}
}

const myVar: Foo = new Bar();