// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/indexerSignatureWithRestParam.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface I {
    [...x]: string;
    //~^ ERROR: An index signature cannot have a rest parameter.
    //~| ERROR: An index signature parameter type must be 'string', 'number', 'symbol', or a template literal type.
}

class C {
    [...x]: string
    //~^ ERROR: An index signature cannot have a rest parameter.
}