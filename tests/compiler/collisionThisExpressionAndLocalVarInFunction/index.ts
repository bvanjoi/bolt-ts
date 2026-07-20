// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionThisExpressionAndLocalVarInFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: lib=[es5]

var console: {
    log(val: any);
}
function x() {
    var _this = 5;
    x => { console.log(this.x); };
}
