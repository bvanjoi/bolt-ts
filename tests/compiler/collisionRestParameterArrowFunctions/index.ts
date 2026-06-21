// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionRestParameterArrowFunctions.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var f1 = (_i: number, ...restParameters) => { //_i is error
    var _i = 10; // no error
}
var f1NoError = (_i: number) => { // no error
    var _i = 10; // no error
}

var f2 = (...restParameters) => {
    var _i = 10; // No Error
}
var f2NoError = () => {
    var _i = 10; // no error
}