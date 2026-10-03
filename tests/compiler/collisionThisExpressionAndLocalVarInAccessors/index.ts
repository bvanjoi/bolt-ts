// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionThisExpressionAndLocalVarInAccessors.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

class class1 {
    get a(): number {
        var x2 = {
            doStuff: (callback) => () => {
                var _this = 2;
                return callback(this);
            }
        }

        return 10;
    }
    set a(val: number) {
        var x2 = {
            doStuff: (callback) => () => {
                var _this = 2;
                return callback(this);
            }
        }

    }
}

class class2 {
    get a(): number {
        var _this = 2;
        var x2 = {
            doStuff: (callback) => () => {
                return callback(this);
            }
        }

        return 10;
    }
    set a(val: number) {
        var _this = 2;
        var x2 = {
            doStuff: (callback) => () => {
                return callback(this);
            }
        }

    }
}