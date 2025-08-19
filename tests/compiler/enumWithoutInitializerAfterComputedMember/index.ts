// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/enumWithoutInitializerAfterComputedMember.ts`, Apache-2.0 License

enum E {
    a,
    b = a,
    c
}

enum B {
    a,
    b = '',
    c = b,
    d
    //~^ ERROR: Enum member must have initializer.
}

enum C {
    a,
    b = '',
    c
    //~^ ERROR: Enum member must have initializer.
}