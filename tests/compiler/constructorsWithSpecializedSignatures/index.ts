// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/constructorsWithSpecializedSignatures.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

// errors
declare class C {
    constructor(x: "hi");
    constructor(x: "foo");
    constructor(x: number);
}

// ok
declare class C2 {
    constructor(x: "hi");
    constructor(x: "foo");
    constructor(x: string);
}

// errors
class D {
    constructor(x: "hi");
    constructor(x: "foo");
    //~^ ERROR: This overload signature is not compatible with its implementation signature.
    constructor(x: number);
    constructor(x: "hi") { }
}

// overloads are ok
class D2 {
    constructor(x: "hi");
    constructor(x: "foo");
    //~^ ERROR: This overload signature is not compatible with its implementation signature.
    constructor(x: string);
    constructor(x: "hi") { } // error
}

// errors
interface I {
    new (x: "hi");
    new (x: "foo");
    new (x: number);
}

// ok
interface I2 {
    new (x: "hi");
    new (x: "foo");
    new (x: string);
}