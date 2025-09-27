// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/typeInferenceConflictingCandidates.ts`, Apache-2.0 License

declare function g<T>(a: T, b: T, c: (t: T) => T): T;

g("", 3, a => a);
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type '""'.