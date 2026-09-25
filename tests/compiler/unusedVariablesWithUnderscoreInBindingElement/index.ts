// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedVariablesWithUnderscoreInBindingElement.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals

function t1() {
    const [_a1, b1] = [1, 2];
    console.log(b1);

    const [a2, _b2] = [1, 2];
    console.log(a2);

    const [_a3, _b3] = [1, 2];
}

function t2() {
    const [_a1, b1] = [1, 2];
    //~^ ERROR: 'b1' is declared but its value is never read.
    const [a2, _b2] = [1, 2];
    //~^ ERROR: 'a2' is declared but its value is never read.
    const [a3, b3] = [1, 2];
    //~^ ERROR: 'a3' is declared but its value is never read.
    //~| ERROR: 'b3' is declared but its value is never read.
}

function t3() {
    const [_a1, [[_b1, c1]], d1, _e1] = [1, [[2, 3]], 4, 5];
    console.log(c1, d1);

    const [_a2, [[_b2, _c2]], _d2, _e2] = [1, [[2, 3]], 4, 5];

    const [a3, [[b3, c3]], d3, e3] = [1, [[2, 3]], 4, 5];
    //~^ ERROR: 'a3' is declared but its value is never read.
    //~| ERROR: 'b3' is declared but its value is never read.
    //~| ERROR: 'c3' is declared but its value is never read.
    //~| ERROR: 'd3' is declared but its value is never read.
    //~| ERROR: 'e3' is declared but its value is never read.
}

function t4() {
    const { a1: _a1, b1 } = { a1: 1, b1: 1 };
    console.log(b1);

    const { a2, b2: _b2 } = { a2: 1, b2: 1 };
    console.log(a2);

    const { a3: _a3, b3: _b3 } = { a3: 1, b3: 1 };
}

function t5() {
    const { a1: _a1, b1 } = { a1: 1, b1: 1 };
    //~^ ERROR: 'b1' is declared but its value is never read.
    const { a2, b2: _b2 } = { a2: 1, b2: 1 };
    //~^ ERROR: 'a2' is declared but its value is never read.
    const { a3, b3 } = { a3: 1, b3: 1 };
    //~^ ERROR: All destructured elements are unused.
}

function t6() {
    const {
        a1: _a1,
        b1: {
            b11: {
                b111: _b111,
                b112
            }
        },
        c1,
        d1: _d1
    } = { a1: 1, b1: { b11: { b111: 1, b112: 1 } }, c1: 1, d1: 1 };
    console.log(b112, c1);

    const {
        a2: _a2,
        b2: {
            b21: {
                b211: _b211, b212: _b212
            }
        },
        c2: _c2,
        d2: _d2
    } = { a2: 1, b2: { b21: { b211: 1, b212: 1 } }, c2: 1, d2: 1 };

    const {
        a3, //~ERROR: 'a3' is declared but its value is never read.
        b3: {
            b31: {    //~ERROR: All destructured elements are unused.
                b311, b312
            }
        },
        c3, //~ERROR: 'c3' is declared but its value is never read.
        d3  //~ERROR: 'd3' is declared but its value is never read.
    } = { a3: 1, b3: { b31: { b311: 1, b312: 1 } }, c3: 1, d3: 1 };
}

function t7() {
    // error
    const { _a1, _b1 } = { _a1: 1, _b1: 1 };
    //~^ ERROR: All destructured elements are unused.

    // ok
    const { a2: _a2, b2: _b2 } = { a2: 1, b2: 1 };

    // ok
    const { _a3: _ignoreA3, _b3: _ignoreB3 } = { _a3: 1, _b3: 1 };
}
