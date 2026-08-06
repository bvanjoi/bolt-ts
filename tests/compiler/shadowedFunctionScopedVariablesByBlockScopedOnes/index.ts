// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/shadowedFunctionScopedVariablesByBlockScopedOnes.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noEmit


// https://github.com/microsoft/TypeScript/issues/2185

function test1() {
    for (let v; ; ) { var v; }
    //~^ ERROR: Cannot initialize outer scoped variable 'v' in the same scope as block scoped declaration 'v'.
}
function test2() {
    for (let v in []) { var v; }
    //~^ ERROR: Cannot initialize outer scoped variable 'v' in the same scope as block scoped declaration 'v'.
}
function test3() {
    for (let v of []) { var v; }
    //~^ ERROR: Cannot initialize outer scoped variable 'v' in the same scope as block scoped declaration 'v'.
}
function test4() {
    {
        let x;
        {
            var x;
            //~^ ERROR: Cannot initialize outer scoped variable 'x' in the same scope as block scoped declaration 'x'.
        }
    }
}
function test5() {
    {
        {
            var x;
            //~^ ERROR: Cannot initialize outer scoped variable 'x' in the same scope as block scoped declaration 'x'.
        }
        let x;
    }
}