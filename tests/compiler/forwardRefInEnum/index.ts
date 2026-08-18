// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/forwardRefInEnum.ts`, Apache-2.0 License

//@compiler-options: target=es2015

enum E1 {
    // illegal case
    // forward reference to the element of the same enum
    X = Y, 
    //~^ ERROR: A member initializer in a enum declaration cannot reference members declared after it, including members defined in other enums.
    X1 = E1["Y"], 
    // forward reference to the element of the same enum
    //~^^ ERROR: A member initializer in a enum declaration cannot reference members declared after it, including members defined in other enums.
    Y = E1.Z,
    //~^ ERROR: A member initializer in a enum declaration cannot reference members declared after it, including members defined in other enums.
    Y1 = E1["Z"]
    //~^ ERROR: A member initializer in a enum declaration cannot reference members declared after it, including members defined in other enums.
}

enum E1 {
    Z = 4    
}
