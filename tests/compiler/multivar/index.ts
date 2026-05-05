// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/multivar.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
var a,b,c;
var x=1,y=2,z=3;

namespace m2 {

    export var a, b2: number = 10, b;
    //~^ ERROR: Individual declarations in merged declaration 'b2' must be all exported or all local.
    var m1;
    var a2, b22: number = 10, b222;
    var m3;

    class C {
        constructor (public b) {
        }
    }

    export class C2 {
        constructor (public b) {
        }
    }
    var m;
    declare var d1, d2;
    var b2;
    //~^ ERROR: Individual declarations in merged declaration 'b2' must be all exported or all local.

    declare var v1;
}

var d;
var a22, b22 = 10, c22 = 30, dn;
var nn;

declare var da1, da2;
var normalVar;
declare var dv1;
var xl;
var x3;
var z4;

function foo(a2) {
    var a = 10;
}


for (var i = 0; i < 30; i++) {
    i++;
}
var b5 = 10;