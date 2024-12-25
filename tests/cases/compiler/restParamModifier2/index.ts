// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/restParamModifier2.ts`, Apache-2.0 License

class C {
    constructor(public ...rest: string[]) {}
    //~^ ERROR: 'public' cannot be declared using a rest parameter.
}