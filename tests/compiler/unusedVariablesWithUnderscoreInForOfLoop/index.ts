// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedVariablesWithUnderscoreInForOfLoop.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals

function t1() {
    for (const [_a, b] of [['key', 1]]) {
        console.log(b);
    }

    for (const [a, _b] of [['key', 1]]) {
        console.log(a);
    }

    for (const [_a, _b] of [['key', 1]]) {}
}


function t2() {
    for (const [_a, b] of [['key', 1]]) {}
    //~^ ERROR: 'b' is declared but its value is never read.

    for (const [a, _b] of [['key', 1]]) {}
    //~^ ERROR: 'a' is declared but its value is never read.

    for (const [a, b] of [['key', 1]]) {}
    //~^ ERROR: 'a' is declared but its value is never read.
    //~| ERROR: 'b' is declared but its value is never read.
}

function t3() {
    for (const [[[_a, b]]] of [[[['key', 1]]]]) {}
    //~^ ERROR: 'b' is declared but its value is never read.

    for (const [[[a, _b]]] of [[[['key', 1]]]]) {}
    //~^ ERROR: 'a' is declared but its value is never read.

    for (const [[[a, b]]] of [[[['key', 1]]]]) {}
    //~^ ERROR: 'a' is declared but its value is never read.
    //~| ERROR: 'b' is declared but its value is never read.
}
