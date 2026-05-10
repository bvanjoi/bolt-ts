// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadOnConstNoAnyImplementation.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function x1(a: number, cb: (x: 'hi') => number);
  //~^ ERROR: This overload signature is not compatible with its implementation signature.
function x1(a: number, cb: (x: 'bye') => number);
function x1(a: number, cb: (x: string) => number) {
    cb('hi');
    cb('bye');
    var hm = 'hm';
    cb(hm);
    cb('uh');
    cb(1); // error
    //~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
}

var cb: (number) => number = (x: number) => 1;
x1(1, cb);
x1(1, (x: 'hi') => 1); // error
x1(1, (x: string) => 1);