// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/duplicateObjectLiteralProperty.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

var x = {
    a: 1,
    b: true, // OK
    a: 56,   // Duplicate
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
    \u0061: "ss", // Duplicate
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
    a: {
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
        c: 1,
        "c": 56, // Duplicate
        //~^ ERROR: An object literal cannot have multiple properties with the same name.
    }
};


var y = {
    get a() { return 0; },
    set a(v: number) { },
    get a() { return 0; }
    //~^ ERROR: Duplicate identifier 'a'.
};