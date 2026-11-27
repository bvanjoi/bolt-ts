// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/capturedParametersInInitializers2.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

function foo(
    y = class {
        static c = x;
        //~^ ERROR: Parameter 'y' cannot reference identifier 'x' declared after it.
        get [x]() {return x;}
        //~^ ERROR: Parameter 'y' cannot reference identifier 'x' declared after it.
        constructor() { x; }
        [z]() { return z; }
        //~^ ERROR: Parameter 'y' cannot reference identifier 'z' declared after it.
    },
    x = 1,
    z = 2
) {
    y.c
}
function foo2(y = class {[x] = x}, x = 1) {
        //~^ ERROR: Parameter 'y' cannot reference identifier 'x' declared after it.
}


function too(
    y = class A {
        get [1]() {
          return 1
        }
        [z]() {  }
        //~^ ERROR: Parameter 'y' cannot reference identifier 'z' declared after it.
    },
    z = 2
) {}