// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noCollisionThisExpressionAndLocalVarInMethod.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var _this = 2;
class a {
    method1() {
        return {
            doStuff: (callback) => () => {
                var _this = 2;
                return callback(_this);
            }
        }
    }
    method2() {
        var _this = 2;
        return {
            doStuff: (callback) => () => {
                return callback(_this);
            }
        }
    }
}