// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/newAbstractInstance.ts`, Apache-2.0 License

abstract class B { }
declare const b: B;
new b();
//~^ ERROR: This expression is not constructable.
