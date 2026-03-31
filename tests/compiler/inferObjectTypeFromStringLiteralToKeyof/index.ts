// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/inferObjectTypeFromStringLiteralToKeyof.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail
declare function inference1<T>(name: keyof T): T;
declare function inference2<T>(target: T, name: keyof T): T;
declare var two: "a" | "d";
const x = inference1(two);
const y = inference2({ a: 1, b: 2, c: 3, d(n) { return n } }, two);
