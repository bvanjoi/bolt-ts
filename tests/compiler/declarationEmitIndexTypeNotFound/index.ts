// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitIndexTypeNotFound.ts`, Apache-2.0 License

//@compiler-options: module=commonjs
//@compiler-options: target=es2015
//@compiler-options: declaration

export interface Test {
    [index: TypeNotFound]: any;
    //~^ ERROR: Cannot find name 'TypeNotFound'.
    //~| ERROR: An index signature parameter type must be 'string', 'number', 'symbol', or a template literal type.
}