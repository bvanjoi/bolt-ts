// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericFunctions1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

function foo<T > (x: T) { return x; }

var x = foo(5); // 'x' should be number
