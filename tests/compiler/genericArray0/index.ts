// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericArray0.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

var x:number[];


var y = x; 

function map<U>() {
    var ys: U[] = [];
}
