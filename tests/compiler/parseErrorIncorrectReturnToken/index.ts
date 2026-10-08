// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parseErrorIncorrectReturnToken.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

type F1 = {
    (n: number) => string; // should be : not =>
    //~^ ERROR: Property or signature expected.
}

type F2 = (n: number): string; // should be => not :
//~^ ERROR: Cannot find name 'string'.
//~| ERROR: Expected '=>'.
//~| ERROR: Identifier expected.
//~| ERROR: Declaration or statement expected.

// doesn't work in non-type contexts, where the return type is optional
let f = (n: number) => string => n.toString();

let o = {
    m(n: number) => string {
//~^ ERROR: Expected '{'.
//~| ERROR: Declaration or statement expected.
//~| ERROR: '}' expected.
//~| ERROR: Expected ','.
//~| ERROR: Property assignment expected.
//~| ERROR: '}' expected.
//~| ERROR: Declaration or statement expected.
//~| ERROR: Unexpected keyword or identifier.
//~| ERROR: Cannot find name 'string'.
        return n.toString();
        //~^ ERROR: A 'return' statement can only be used within a function body.
        //~| ERROR: Cannot find name 'n'.
    }
};//~ERROR: Declaration or statement expected.
