// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeofProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I1 {
    a: number;
    b: typeof a; // Should yield error (a is not a value)
    //~^ ERROR: Cannot find name 'a'.
}

interface I2 {
    c: typeof d; // Should yield error (d is not a value)
    //~^ ERROR: Cannot find name 'd'.
    d: string;
}

interface I3 {
    e: typeof e; // Should yield error (e is not a value)
    //~^ ERROR: Cannot find name 'e'.
}

class C1 {
    a: number;
    b: typeof a; // Should yield error (a is not a value)
    //~^ ERROR: Cannot find name 'a'.
}


class C2 {
    c: typeof d; // Should yield error (d is not a value)
    //~^ ERROR: Cannot find name 'd'.
    d: string;
}

class C3 {
    e: typeof e; // Should yield error (e is not a value)
    //~^ ERROR: Cannot find name 'e'.
}



interface ValidInterface {
    x: string;
}

class ValidClass implements ValidInterface {
    x: string;
}

var vcInstance = new ValidClass();
var viInstance = vcInstance;

var x1: typeof vcInstance.x; // x1: string
var x2: typeof viInstance.x; // x2: string
var x3: typeof vcInstance.x = 42;
//~^ ERROR: Type 'number' is not assignable to type 'string'.
var x4: typeof viInstance.x = 42;
//~^ ERROR: Type 'number' is not assignable to type 'string'.
