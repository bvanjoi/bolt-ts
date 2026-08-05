// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/createArray.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var na=new number[];
//~^ ERROR: An element access expression should take an argument.
//~| ERROR: Cannot find name 'number'.

class C {
}

new C[];
//~^ ERROR: An element access expression should take an argument.
var ba=new boolean[];
//~^ ERROR: Cannot find name 'boolean'.
//~| ERROR: An element access expression should take an argument.
var sa=new string[];
//~^ ERROR: Cannot find name 'string'.
//~| ERROR: An element access expression should take an argument.
function f(s:string):number { return 0;
}
if (ba[14]) {
    na[2]=f(sa[3]);
}

new C[1]; // not an error
