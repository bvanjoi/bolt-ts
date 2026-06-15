// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionThisExpressionAndLocalVarInLambda.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

declare function alert(message?: any): void;

var x = {
    doStuff: (callback) => () => {
        var _this = 2;
        return callback(this);
    }
}
alert(x.doStuff(x => alert(x)));
