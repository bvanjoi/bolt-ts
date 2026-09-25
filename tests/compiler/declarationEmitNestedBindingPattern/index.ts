// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitNestedBindingPattern.ts`, Apache-2.0 License

//@compiler-options: declaration

// Nested array binding pattern
export class C1 {
    constructor(public [[x]]: any[]) {}
    //~^ ERROR: A parameter property may not be declared using a binding pattern.
}

// Nested object binding pattern
export class C2 {
    constructor(public [{y}]: any[]) {}
    //~^ ERROR: A parameter property may not be declared using a binding pattern.
}

// Multiple levels of array nesting
export class C3 {
    constructor(public [[[z]]]: any[]) {}
    //~^ ERROR: A parameter property may not be declared using a binding pattern.
}

// Mixed array and object nesting
export class C4 {
    constructor(public [{a: [b]}]: any[]) {}
    //~^ ERROR: A parameter property may not be declared using a binding pattern.
}

// Object with nested array
export class C5 {
    constructor(public {prop: [c]}: any) {}
    //~^ ERROR: A parameter property may not be declared using a binding pattern.
}

// Object with multiple nested levels
export class C6 {
    constructor(public {prop: {nested: [d]}}: any) {}
    //~^ ERROR: A parameter property may not be declared using a binding pattern.
}

// Multiple parameters with nested patterns
export class C7 {
    constructor(
        public [[e]]: any[],
    //~^ ERROR: A parameter property may not be declared using a binding pattern.
        public [{f}]: any[]
    //~^ ERROR: A parameter property may not be declared using a binding pattern.
    ) {}
}

// Nested pattern with rest element
export class C8 {
    constructor(public [[g, ...rest]]: any[]) {}
    //~^ ERROR: A parameter property may not be declared using a binding pattern.
}

// Complex nested pattern
export class C9 {
    constructor(public [[h, i], {j, k: [l]}]: any) {}
    //~^ ERROR: A parameter property may not be declared using a binding pattern.
}

