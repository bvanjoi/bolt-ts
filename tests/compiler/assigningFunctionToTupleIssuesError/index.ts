// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assigningFunctionToTupleIssuesError.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare let a: () => void;
let b: [string] = a;
//~^ ERROR: Type '() => void' is not assignable to type '[string]'.