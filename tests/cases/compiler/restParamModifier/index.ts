// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/restParamModifier.ts`, Apache-2.0 License

class C {
    constructor(...public rest: string[]) {}
    //~^ ERROR: Expected ','.
}