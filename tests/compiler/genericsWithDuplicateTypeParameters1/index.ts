// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/genericsWithDuplicateTypeParameters1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function f<x, x>() { }  //~ERROR: Duplicate identifier 'x'.
function f2<X, X>(a: X, b: X): X { return null; }   //~ERROR: Duplicate identifier 'X'.
class C<X, X> {                                     //~ERROR: Duplicate identifier 'X'.
    public f<x, x>() {}   //~ERROR: Duplicate identifier 'x'.
    public f2<X, X>(a: X, b: X): X { return null; } //~ERROR: Duplicate identifier 'X'.
}

interface I<X, X> {                         //~ERROR: Duplicate identifier 'X'.
    f<X, X>();                              //~ERROR: Duplicate identifier 'X'.
    f2<X, X>(a: X, b: X): X;                //~ERROR: Duplicate identifier 'X'.
}

var m = {
    a: function f<X, X>() {},                               //~ERROR: Duplicate identifier 'X'.
    b: function f2<X, X>(a: X, b: X): X { return null; }    //~ERROR: Duplicate identifier 'X'.
}