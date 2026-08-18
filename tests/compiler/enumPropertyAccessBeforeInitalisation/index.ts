// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/enumPropertyAccessBeforeInitalisation.ts`, Apache-2.0 License

enum E {
    A = A,
    //~^ ERROR: Property 'A' is used before being assigned.
    B = E.B,
    //~^ ERROR: Property 'B' is used before being assigned.
    C = E["C"],
    //~^ ERROR: Property 'C' is used before being assigned.
    D = 1 + D 
    //~^ ERROR: Property 'D' is used before being assigned.
}
