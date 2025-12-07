// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/constructorOverloads8.ts`, Apache-2.0 License

class C {
    constructor(x) { }
    //~^ ERROR: Multiple constructor implementations are not allowed.
    constructor(y, x) { } // illegal, 2 constructor implementations
    //~^ ERROR: Multiple constructor implementations are not allowed.
}

class D {
    constructor(x: number);
    constructor(y: string); // legal, overload signatures for 1 implementation
    constructor(x) { }
}

interface I {
    new (x);
    new (x, y); // legal, overload signatures for (presumably) 1 implementation
}