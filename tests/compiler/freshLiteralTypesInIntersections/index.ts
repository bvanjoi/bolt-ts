// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/freshLiteralTypesInIntersections.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

declare function func<A extends string, B extends A>(a: A, b: B[]): (ab: A & B) => void;
const q = func("x" as "x" | "y", ["x"]);
q("x");
