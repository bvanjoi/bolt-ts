// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/null.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noUncheckedIndexedAccess

var x=null; 
var y=3+x;  
var z=3+null; 
//~^ ERROR: Operator '+' cannot be applied to types '3' and 'null'.
class C {
}
function f() {
    return null;
    return new C();
}
function g() {
    return null;
    return 3;
}
interface I {
    x:any;
    y:number;
}
var w:I={x:null,y:3};


