// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/arrowFunctionErrorSpan.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function f(a: () => number) { }

// oneliner
f(() => { });
//~^ ERROR: Argument of type '() => void' is not assignable to parameter of type '() => number'.

// multiline, body
f(() => {

});
//~^^^ ERROR: Argument of type '() => void' is not assignable to parameter of type '() => number'.

// multiline 2, body
f(() => {

});
//~^^^ ERROR: Argument of type '() => void' is not assignable to parameter of type '() => number'.

// multiline 3, arrow on a new line
f(()
    => { });
//~^^ ERROR: Argument of type '() => void' is not assignable to parameter of type '() => number'.

// multiline 4, arguments
f((a,
    b,
    c,
    d) => { });
//~^^^^ ERROR: Argument of type '(a: any, b: any, c: any, d: any) => void' is not assignable to parameter of type '() => number'.

// single line with a comment
f(/*
    */() => { });
    //~^ ERROR: Argument of type '() => void' is not assignable to parameter of type '() => number'.
// multi line with a comment
f(/*
    */() => { });
    //~^ ERROR: Argument of type '() => void' is not assignable to parameter of type '() => number'.
// multi line with a comment 2
f(/*
    */() => { 

    });
    //~^^^ ERROR: Argument of type '() => void' is not assignable to parameter of type '() => number'.

// multi line with a comment 3
f(  // comment 1
    // comment 2
    () =>
    // comment 3
    {
        // comment 4
    }
    // comment 5
    //~^^^^^^ ERROR: Argument of type '() => void' is not assignable to parameter of type '() => number'.
); 

// body is not a block
f(_ => 1 +
    2);
//~^^ ERROR: Argument of type '(_: any) => number' is not assignable to parameter of type '() => number'.