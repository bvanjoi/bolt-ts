// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedLocalsAndObjectSpread.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es5]
//@compiler-options: noUnusedLocals

declare var console: { log(a: any): void };

function one() {
    const foo = { a: 1, b: 2 };
    // 'a' is declared but never used
    const {a, ...bar} = foo;
    console.log(bar);
}

function two() {
    const foo = { a: 1, b: 2 };
    // '_' is declared but never used
    const {a: _, ...bar} = foo;
    console.log(bar);
}

function three() {
    const foo = { a: 1, b: 2 };
    // 'a' is declared but never used
    const {a, ...bar} = foo; // bar should be unused
    //~^ ERROR: 'bar' is declared but its value is never read.
    //console.log(bar);
}

function four() {
    const foo = { a: 1, b: 2 };
    // '_' is declared but never used
    const {a: _, ...bar} = foo; // bar should be unused
    //~^ ERROR: 'bar' is declared but its value is never read.
    //console.log(bar);
}
