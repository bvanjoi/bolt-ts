// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/genericAndNonGenericInheritedSignature1.ts`, Apache-2.0 License

interface Foo {
    f(x: any): any;
}
interface Bar {
    f<T>(x: T): T;
}
interface Hello extends Foo, Bar {
//~^ ERROR: Interface 'Hello' cannot simultaneously extend types 'Foo' and 'Bar'.
}
