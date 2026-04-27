// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/unionTypeWithLeadingOperator.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

type A = | string;
type B =
  | { type: "INCREMENT" }
  | { type: "DECREMENT" };

type C = [| 0 | 1, | "foo" | "bar"];

export type D = 
  /*leading0*/
  | /*leading1*/ 1 /*trailing1*/ 
  | /*leading2*/ 2 /*trailing2*/;