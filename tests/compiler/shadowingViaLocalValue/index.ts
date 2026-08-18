// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/shadowingViaLocalValue.ts`, Apache-2.0 License

//@compiler-options: target=es2015

{
    let x;
    {
        var x = 1;
        //~^ ERROR: Cannot initialize outer scoped variable 'x' in the same scope as block scoped declaration 'x'.
    }
}

{
    let x1;
    {
        for (var x1 = 0; ;);
        //~^ ERROR: Cannot initialize outer scoped variable 'x1' in the same scope as block scoped declaration 'x1'.
    }
}