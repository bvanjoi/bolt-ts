// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/didYouMeanStringLiteral.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type T1 = "string" | "number" | "boolean";
type T2 = T1 & ("number" | "boolean"); // "number" | "boolean"
type T3 = T1 & ("string" | "boolean"); // "string" | "boolean"

const t1: T1 = "strong";
//~^ ERROR: Type 'string' is not assignable to type 'T1'.
const t2: T2 = "strong";
//~^ ERROR: Type 'string' is not assignable to type '"number" | "boolean"'.
const t3: T3 = "strong";
//~^ ERROR: Type 'string' is not assignable to type '"string" | "boolean"'.
