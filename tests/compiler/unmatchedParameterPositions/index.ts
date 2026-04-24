// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unmatchedParameterPositions.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=true
//@run-fail

declare let s: (...items: never[]) => never[];
let t1: () => unknown[] = s;
let t2: (...args: []) => unknown[] = s;
