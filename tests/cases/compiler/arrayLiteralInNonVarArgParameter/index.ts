// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/arrayLiteralInNonVarArgParameter.ts`, Apache-2.0 License

function panic(val: string[], ...opt: string[]) { }

panic([], 'one', 'two');
