// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/enumWithComputedMember.ts`, Apache-2.0 License

enum A { 
    X = "".length, 
    Y = X,
    Z
    //~^ ERROR: Enum member must have initializer.
}
