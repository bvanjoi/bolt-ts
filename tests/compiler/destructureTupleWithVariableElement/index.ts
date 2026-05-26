// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructureTupleWithVariableElement.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit
//@compiler-options: noUncheckedIndexedAccess

type NonEmptyStringArray = [string, ...Array<string>]

const strings: NonEmptyStringArray = ['one', 'two']
const [s0, s1, s2] = strings;

s0.toUpperCase()
s1.toUpperCase() 
//~^ ERROR: 's1' is possibly 'undefined'.
s2.toUpperCase()
//~^ ERROR: 's2' is possibly 'undefined'.

declare const strings2: [string, ...Array<string>, string]

const [s3, s4, s5] = strings2;

s3.toUpperCase()
s4.toUpperCase() 
s5.toUpperCase()
//~^ ERROR: 's5' is possibly 'undefined'.
