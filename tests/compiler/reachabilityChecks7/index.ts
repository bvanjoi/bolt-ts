// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reachabilityChecks7.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@compiler-options: target=ES6
//@compiler-options: noImplicitReturns

// async function without return type annotation - error
async function f1() {    
}

let x = async function() {
}

// async function with which promised type is void - return can be omitted
async function f2(): Promise<void> {
    
}

async function f3(x) {
  //~^ ERROR: Not all code paths return a value.
    if (x) return 10;
}

async function f4(): Promise<number> {
  //~^ ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.
    
}

function voidFunc(): void {
}

function calltoVoidFunc(x) {
    if (x) return voidFunc();
}

declare function use(s: string): void;
let x1 = () => { use("Test"); }