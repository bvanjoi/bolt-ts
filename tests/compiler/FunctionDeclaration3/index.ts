// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/FunctionDeclaration3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo();
//~^ ERROR: Function implementation is missing or not immediately following the declaration.
//~| ERROR: 'foo', which lacks return-type annotation, implicitly has an 'any' return type.