// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/spreadObjectWithIndexDoesNotAddUndefinedToLocalIndex.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

declare const m: { [k: string]: string };
const x: { [k: string]: string } = { ...m, ["a" + "b"]: "" };
