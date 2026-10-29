// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/expandoFunctionContextualTypesJSDocInTs.ts`, Apache-2.0 License

//@compiler-options: target=es2015
export function Foo() { }

// This comment should have no effect; this is a TS file.
/** @type {never} */
Foo.bar = () => { };
