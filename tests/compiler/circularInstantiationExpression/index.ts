// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/circularInstantiationExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration
//@run-fail

declare function foo<T>(t: T): typeof foo<T>;
foo("");
