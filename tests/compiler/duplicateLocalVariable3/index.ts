// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/duplicateLocalVariable3.ts`, Apache-2.0 License

var x = 1;
var x = 2; 

function f() {
    var y = 1;
    var y = 2;
}

function f2() {
    var z = 3;
    var z = "";
    //~^ ERROR: Subsequent variable declarations must have the same type. Variable 'z' must be of type 'number', but here has type 'string'.
}
