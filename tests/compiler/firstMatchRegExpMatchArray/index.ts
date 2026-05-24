// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/firstMatchRegExpMatchArray.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noUncheckedIndexedAccess

const match = ''.match(/ /)

if (match !== null) {
    const foo: string = match[0]
    const bar: string = match[1]
    //~^ ERROR: Type 'undefined | string' is not assignable to type 'string'.
}