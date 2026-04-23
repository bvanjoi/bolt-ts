// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/declareModifierOnImport1.ts`, Apache-2.0 License

//@compiler-options: target=es6
declare import a = b;
//~^ ERROR: Cannot find name 'b'.
//~| ERROR: A 'declare' modifier cannot be used with an import declaration.