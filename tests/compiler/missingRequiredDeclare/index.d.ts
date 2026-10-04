// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/missingRequiredDeclare.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var x = 1;
//~^ ERROR: Top-level declarations in .d.ts files must start with either a 'declare' or 'export' modifier.
//~| ERROR: Initializers are not allowed in ambient contexts.
