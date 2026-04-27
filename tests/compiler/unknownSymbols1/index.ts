// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/unknownSymbols1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var x = asdf; //~ERROR: Cannot find name 'asdf'.
var y: asdf;  //~ERROR: Cannot find name 'asdf'.

function foo(x: asdf, y: number): asdf { }  
//~^ ERROR: Cannot find name 'asdf'.
//~| ERROR: Cannot find name 'asdf'.
function foo2() {
    return asdf; //~ERROR: Cannot find name 'asdf'.
}

var z = <asdf>x; // should be an error
//~^ ERROR: Cannot find name 'asdf'.

class C<T> {
    foo: asdf;    //~ERROR: Cannot find name 'asdf'.
    bar: C<asdf>; //~ERROR: Cannot find name 'asdf'.
}

class C2 implements asdf { }
//~^ ERROR: Cannot find name 'asdf'.
interface I extends adsf { }
//~^ ERROR: Cannot find name 'adsf'.

class C3 { constructor(x: any) { } }
class C4 extends C3 {
    constructor() {
        super(asdf);
        //~^ ERROR: Cannot find name 'asdf'.
    }
}

var x2 = this.asdf; // no error, this is any

class C5 {
    constructor() {
        this.asdf = asdf;
//~^ ERROR: Cannot find name 'asdf'.
//~| ERROR: Property 'asdf' does not exist on type 'C5'.
    }
}