// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parseCommaSeparatedNewlineNumber.ts`, Apache-2.0 License

//@compiler-options: target=es2015
(a,
1)
//~^^ ERROR: Cannot find name 'a'.
//~| ERROR: Left side of comma operator is unused and has no side effects.