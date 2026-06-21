// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/doNotInferUnrelatedTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

declare function dearray<T>(ara: ReadonlyArray<T>): T;
type LiteralType = "foo" | "bar";
declare var alt: Array<LiteralType>;

let foo: LiteralType = dearray(alt);
