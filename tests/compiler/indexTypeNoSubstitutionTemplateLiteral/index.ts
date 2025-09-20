// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/indexTypeNoSubstitutionTemplateLiteral.ts`, Apache-2.0 License

//@compiler-options: strict

function Foo() {}
Foo[`b`] = function () {};

type Test = keyof typeof Foo;

