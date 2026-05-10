// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constDeclarations-scopes2.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: strict=false

// global
const c = "string";

var n: number;
var b: boolean;

// for scope
for (const c = 0; c < 10; n = c ) {
    // for block
    const c = false;
    b = c;
    
}

n = c;
//~^ ERROR: Type 'string' is not assignable to type 'number'.