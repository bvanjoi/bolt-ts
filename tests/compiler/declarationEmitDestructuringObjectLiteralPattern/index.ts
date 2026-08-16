// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitDestructuringObjectLiteralPattern.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

var { } = { x: 5, y: "hello" };
var { x4 } = { x4: 5, y4: "hello" };
//~^ ERROR: Object literal may only specify known properties, and 'y4' does not exist in type '{ x4: any; }'.
var { y5 } = { x5: 5, y5: "hello" };
//~^ ERROR: Object literal may only specify known properties, and 'x5' does not exist in type '{ y5: any; }'.
var { x6, y6 } = { x6: 5, y6: "hello" };
var { x7: a1 } = { x7: 5, y7: "hello" };
//~^ ERROR: Object literal may only specify known properties, and 'y7' does not exist in type '{ x7: any; }'.
var { y8: b1 } = { x8: 5, y8: "hello" };
//~^ ERROR: Object literal may only specify known properties, and 'x8' does not exist in type '{ y8: any; }'.
var { x9: a2, y9: b2 } = { x9: 5, y9: "hello" };

var { a: x11, b: { a: y11, b: { a: z11 }}} = { a: 1, b: { a: "hello", b: { a: true } } };

function f15() {
    var a4 = "hello";
    var b4 = 1;
    var c4 = true;
    return { a4, b4, c4 };
}
var { a4, b4, c4 } = f15();

namespace m {
    export var { a4, b4, c4 } = f15();
}
