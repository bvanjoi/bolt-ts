// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionOverloads12.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: allowUnreachableCode

function foo():string;
function foo():number;
function foo():any { if (true) return ""; else return 0;}
