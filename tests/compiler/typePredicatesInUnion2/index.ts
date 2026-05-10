// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/typePredicatesInUnion2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

declare function isString(x: any): x is string;
declare function isNumber(x: any): x is number;
declare function f(p: typeof isString | typeof isNumber): void;
f(isString);
f(isNumber);
