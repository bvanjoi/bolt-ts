// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/parameterReferenceInInitializer2.ts`, Apache-2.0 License

function Example(x = function(x: any) { return x; }) {}