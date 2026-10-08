// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeAliasDeclareKeywordNewlines.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare var declare: string;
declare var type: number;

// The following is invalid but should declare a type alias named 'T1':
declare type /*unexpected newline*/
T1 = null;    //~ERROR: Line break not permitted here.
const t1: T1 = null; // Assert that T1 is the null type.

let T: null = null;
// The following should use a variable named 'declare', use a variable named
// 'type', and assign to a variable named 'T'.
declare /*ASI*/
type /*ASI*/
T = null;

// The following should use a variable named 'declare' and declare a type alias
// named 'T2':
declare /*ASI*/
type T2 = null;
const t2: T2 = null; // Assert that T2 is the null type.
