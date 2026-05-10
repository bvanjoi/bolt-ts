// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noCollisionThisExpressionAndLocalVarInConstructor.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class class1 {
    constructor() {
        var x2 = {
            doStuff: (callback) => () => {
                var _this = 2;
                return callback(_this);
            }
        }
    }
}

class class2 {
    constructor() {
        var _this = 2;
        var x2 = {
            doStuff: (callback) => () => {
                return callback(_this);
            }
        }
    }
}