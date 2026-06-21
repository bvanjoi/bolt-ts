// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowArrayErrors.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

declare function cond(): boolean;

function f1() {
    let x = [];  // Implicit any[] error in some locations
    //~^ ERROR: Variable 'x' implicitly has type 'any[]' in some locations where its type cannot be determined.
    let y = x;   // Implicit any[] error
    //~^ ERROR: Variable 'x' implicitly has an 'any[]' type.
    x.push(5);
    let z = x;
}

function f2() {
    let x;       // Implicit any[] error in some locations
    //~^ ERROR: Variable 'x' implicitly has type 'any[]' in some locations where its type cannot be determined.
    x = [];
    let y = x;   // Implicit any[] error
    //~^ ERROR: Variable 'x' implicitly has an 'any[]' type.
    x.push(5);
    let z = x;
}

function f3() {
    let x = [];  // Implicit any[] error in some locations
    //~^ ERROR: Variable 'x' implicitly has type 'any[]' in some locations where its type cannot be determined.
    x.push(5);
    function g() {
        x;       // Implicit any[] error
        //~^ ERROR: Variable 'x' implicitly has an 'any[]' type.
    }
}

function f4() {
    let x;
    x = [5, "hello"];  // Non-evolving array
    x.push(true);      // Error
    //~^ ERROR: Argument of type 'boolean' is not assignable to parameter of type 'number | string'.
}

function f5() {
    let x = [5, "hello"];  // Non-evolving array
    x.push(true);          // Error
    //~^ ERROR: Argument of type 'boolean' is not assignable to parameter of type 'number | string'.
}

function f6() {
    let x;
    if (cond()) {
        x = [];
        x.push(5);
        x.push("hello");
    }
    else {
        x = [true];  // Non-evolving array
    }
    x;           // boolean[] | (string | number)[]
    x.push(99);  // Error
    //~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'never'.
}

function f7() {
    let x = [];       // x has evolving array value
    x.push(5);
    let y = x;        // y has non-evolving array value
    x.push("hello");  // Ok
    y.push("hello");  // Error
    //~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.
}

function f8() {
    const x = [];  // Implicit any[] error in some locations
    //~^ ERROR: Variable 'x' implicitly has type 'any[]' in some locations where its type cannot be determined.
    x.push(5);
    function g() {
        x;  // Implicit any[] error
        //~^ ERROR: Variable 'x' implicitly has an 'any[]' type.
    }
}