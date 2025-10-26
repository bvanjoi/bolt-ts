// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/collisionThisExpressionAndLocalVarInConstructor.ts`, Apache-2.0 License

class class1 {
    constructor() {
        var x2 = {
            doStuff: (callback) => () => {
                var _this = 2;
                return callback(this);
            }
        }
    }
}

class class2 {
    constructor() {
        var _this = 2;
        var x2 = {
            doStuff: (callback) => () => {
                return callback(this);
            }
        }
    }
}