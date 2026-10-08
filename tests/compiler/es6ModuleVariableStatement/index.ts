// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es6ModuleVariableStatement.ts`, Apache-2.0 License

//@compiler-options: target=es6

export var a = "hello";
export var x: string = a, y = x;
var b = y;
var c: string = b, d = c;
export namespace m1 {
    export var k = a;
    export var l: string = b, m = k;
    var n = m1.k;
    var o: string = n, p = k;
}
namespace m2 {
    export var k = a;
    export var l: string = b, m = k;
    var n = m1.k;
    var o: string = n, p = k;
}