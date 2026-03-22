// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/avoid.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function f() {
    var x=1;
}

var y=f();  // error void fn
var why:any=f(); // error void fn
var w:any;
w=f(); // error void fn

class C {
    g() {
        
    }
}

var z=new C().g(); // error void fn
var N=new f();  // ok with void fn
