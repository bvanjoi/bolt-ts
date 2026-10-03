// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/letDeclarations-scopes-duplicates.ts`, Apache-2.0 License

//@compiler-options: target=ES6

// Errors: redeclaration
let var1 = 0;
let var1 = 0; // error
//~^ ERROR: Duplicate identifier 'var1'.

let var2 = 0;
const var2 = 0;
//~^ ERROR: Duplicate identifier 'var2'.

const var3 = 0;
let var3 = 0;
//~^ ERROR: Duplicate identifier 'var3'.

const var4 = 0;
const var4 = 0;
//~^ ERROR: Duplicate identifier 'var4'.

var var5 = 0;
let var5 = 0;
//~^ ERROR: Duplicate identifier 'var5'.

let var6 = 0;
var var6 = 0;
//~^ ERROR: Duplicate identifier 'var6'.

{
    let var7 = 0;
    let var7 = 0;
    //~^ ERROR: Duplicate identifier 'var7'.
    {
        let var8 = 0;
        const var8 = 0;
        //~^ ERROR: Duplicate identifier 'var8'.
    }
}

switch (0) {
    default:
        let var9 = 0;
        let var9 = 0;
        //~^ ERROR: Duplicate identifier 'var9'.
}

try {
    const var10 = 0;
    const var10 = 0;
    //~^ ERROR: Duplicate identifier 'var10'.
}
catch (e) {
    let var11 = 0;
    let var11 = 0;
    //~^ ERROR: Duplicate identifier 'var11'.
}

function F1() {
    let var12;
    let var12;
    //~^ ERROR: Duplicate identifier 'var12'.
}

// OK
var var20 = 0;

var var20 = 0
{
    let var20 = 0;
    {
        let var20 = 0;
    }
}

switch (0) {
    default:
        let var20 = 0;
}

try {
    let var20 = 0;
}
catch (e) {
    let var20 = 0;
}

function F() {
    let var20;
}

