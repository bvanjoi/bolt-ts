// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyFunctionExpressionAssignment.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

var x: (a: any) => void = function <T>(x: T) {
    return null;
};

var x2: (a: any) => void = function f<T>(x: T) {
    return null;
};