// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeCheckObjectLiteralMethodBody.ts`, Apache-2.0 License

//@compiler-options: noImplicitAny

var foo = { bar() { return undefined } };
//~^ ERROR: 'bar', which lacks return-type annotation, implicitly has an 'any' return type.