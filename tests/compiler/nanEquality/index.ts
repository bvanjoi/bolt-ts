// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nanEquality.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare const x: number;

if (x === NaN) {}
//~^ ERROR: This condition will always return 'false'. 
if (NaN === x) {}
//~^ ERROR: This condition will always return 'false'. 

if (x == NaN) {}
//~^ ERROR: This condition will always return 'false'. 
if (NaN == x) {}
//~^ ERROR: This condition will always return 'false'. 

if (x !== NaN) {}
//~^ ERROR: This condition will always return 'true'. 
if (NaN !== x) {}
//~^ ERROR: This condition will always return 'true'. 

if (x != NaN) {}
//~^ ERROR: This condition will always return 'true'. 
if (NaN != x) {}
//~^ ERROR: This condition will always return 'true'. 

if (x === ((NaN))) {}
//~^ ERROR: This condition will always return 'false'. 
if (((NaN)) === x) {}
//~^ ERROR: This condition will always return 'false'. 

if (x !== ((NaN))) {}
//~^ ERROR: This condition will always return 'true'. 
if (((NaN)) !== x) {}
//~^ ERROR: This condition will always return 'true'. 

if (NaN === NaN) {}
//~^ ERROR: This condition will always return 'false'. 
if (NaN !== NaN) {}
//~^ ERROR: This condition will always return 'true'. 

if (NaN == NaN) {}
//~^ ERROR: This condition will always return 'false'. 
if (NaN != NaN) {}
//~^ ERROR: This condition will always return 'true'. 

// ...
declare let y: any;
if (NaN === y[0][1]) {}
//~^ ERROR: This condition will always return 'false'.

function t1(value: number, NaN: number) {
    return value === NaN; // ok
}

function t2(value: number, NaN: number) {
    return NaN == value; // ok
}

function t3(NaN: number) {
    return NaN === NaN; // ok
}
