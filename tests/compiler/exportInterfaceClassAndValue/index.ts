// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportInterfaceClassAndValue.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export const foo = 1
export declare class foo {}
//~^ ERROR: Duplicate identifier 'foo'.
export interface foo {}
