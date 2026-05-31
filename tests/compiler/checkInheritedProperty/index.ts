// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/checkInheritedProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

class Base {
}

declare const BaseFactory: new() => Base & { c: string }

class Derived extends BaseFactory {
    a = this.b //~ERROR: Property 'b' is used before its initialization.
    b = "abc"
}

class D extends BaseFactory {
    b = "abc"
    a = this.b
}