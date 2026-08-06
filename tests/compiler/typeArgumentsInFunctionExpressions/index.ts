// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeArgumentsInFunctionExpressions.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var obj = function f<T>(a: T) { // should not error
    var x: T;
    return a;
};
 
var obj2 = function f<T>(a: T): T { // should not error
    var x: T;
    return a;
};

