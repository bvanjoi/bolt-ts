// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/letDeclarations-scopes2.ts`, Apache-2.0 License

//@compiler-options: target=ES6

let global = 0;

{ 
    let local = 0;

    local;  // OK
    global; // OK
    local2; // Error
    //~^ ERROR: Cannot find name 'local2'.

    {
        let local2 = 0;

        local;  // OK
        global; // OK
        local2; // OK
    } 

    local;  // OK
    global; // OK
    local2; // Error
    //~^ ERROR: Cannot find name 'local2'.
}

local;  // Error
//~^ ERROR: Cannot find name 'local'.
global; // OK
local2; // Error
//~^ ERROR: Cannot find name 'local2'.
