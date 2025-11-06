// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/genericAndNonGenericInheritedSignature2.ts`, Apache-2.0 License

interface Foo {
    f(x: any): any;
}
interface Bar {
    f<T>(x: T): T;
}
interface Hello extends Bar, Foo {
//~^ ERROR: Interface 'Hello' cannot simultaneously extend types 'Bar' and 'Foo'.
}

