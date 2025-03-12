// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/conditionalTypeGenericInSignatureTypeParameterConstraint.ts`, Apache-2.0 License

// should be x
type H_inline1<x> = (<o extends x>() => o) extends (() => infer o) ? o : never;

type Result = H_inline1<string>; // should be `string`

let a: Result = "42";
let b: Result = "abc";
