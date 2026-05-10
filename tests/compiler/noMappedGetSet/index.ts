// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noMappedGetSet.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type OH_NO = {
    get [K in WAT](): string;
    //~^ ERROR: Cannot find name 'K'.
    //~| ERROR: Cannot find name 'WAT'.
    //~| ERROR: A computed property name must be of type 'string', 'number', 'symbol' or 'any'.
};
