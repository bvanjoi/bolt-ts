// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedDestructuringParameters.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noUnusedParameters

const f = ([a]) => { };
//~^ ERROR: 'a' is declared but its value is never read.
f([1]);
const f2 = ({a}) => { };
//~^ ERROR: 'a' is declared but its value is never read.
f2({ a: 10 });
const f3 = ([_]) => { };
f3([10]);
