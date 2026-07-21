// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/sliceResultCast.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

declare var x: [number, string] | [number, string, string];

x.slice(1) as readonly string[];