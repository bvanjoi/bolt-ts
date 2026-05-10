// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classMemberInitializerScoping.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var aaa = 1;
class CCC {
    y: number = aaa;
    //~^ ERROR: Initializer of instance member variable 'y' cannot reference identifier 'aaa' declared in the constructor.
    static staticY: number = aaa; // This shouldnt be error
    constructor(aaa) {
        this.y = ''; // was: error, cannot assign string to number
        //~^ ERROR: Type 'string' is not assignable to type 'number'.
    }
}

// above is equivalent to this:
var aaaa = 1;
class CCCC {
    y: any;
    constructor(aaaa) {
        this.y = aaaa;
        this.y = '';
    }
}
 