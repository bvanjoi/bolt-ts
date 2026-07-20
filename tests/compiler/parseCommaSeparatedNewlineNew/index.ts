// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parseCommaSeparatedNewlineNew.ts`, Apache-2.0 License

//@compiler-options: target=es2015
(a,
new)
//~^^ ERROR: Cannot find name 'a'.
//~| ERROR: Left side of comma operator is unused and has no side effects.
//~^^^ ERROR: Expression expected.