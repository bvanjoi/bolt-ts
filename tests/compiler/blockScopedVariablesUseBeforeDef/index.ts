// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/blockScopedVariablesUseBeforeDef.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: lib=[esnext,dom]

function foo0() {
    let a = x;
    //~^ ERROR: Block-scoped variable 'x' used before its declaration.
    let x;
}

function foo1() {
    let a = () => x;
    let x;
}

function foo2() {
    let a = function () { return x; }
    let x;
}

function foo3() {
    class X {
        m() { return x;}
    }
    let x;
}

function foo4() {
    let y = class {
        m() { return x; }
    };
    let x;
}

function foo5() {
    let x = () => y;
    let y = () => x;
}

function foo6() {
    function f() {
        return x;
    }
    let x;
}

function foo7() {
    class A {
        a = x;
    }
    let x;
}

function foo8() {
    let y = class {
        a = x;
    }
    let x;
}

function foo9() {
    let y = class {
        static a = x;
        //~^ ERROR: Block-scoped variable 'x' used before its declaration.
    }
    let x;
}

function foo10() {
    class A {
        static a = x;
        //~^ ERROR: Block-scoped variable 'x' used before its declaration.
    }
    let x;
}

function foo11() {
    function f () {
        let y = class {
            static a = x;
        }
    }
    let x;
}

function foo12() {
    function f () {
        let y = class {
            a;
            constructor() {
                this.a = x;
            }
        }
    }
    let x;
}

function foo13() {
    let a = {
        get a() { return x } 
    }
    let x
}

function foo14() {
    let a = {
        a: x 
        //~^ ERROR: Block-scoped variable 'x' used before its declaration.
    }
    let x
}

function foo15() {
    // https://github.com/microsoft/TypeScript/issues/42678
    const [
        a,
        b,
    ] = ((): [number, number] => {
        (() => console.log(a))();  // should error
        //~^ ERROR: Block-scoped variable 'a' used before its declaration.
        console.log(a);            // should error
        //~^ ERROR: Block-scoped variable 'a' used before its declaration.
        const b = () => a;         // should be ok
        return [
            0,
            0,
        ];
    })();    
}

function foo16() {
    let [a] = (() => a)();
    //~^ ERROR: Block-scoped variable 'a' used before its declaration.
    //~| ERROR: Block-scoped variable 'a' used before its declaration.
}

function foo17() {
    const promise = (async () => {
        promise
        foo
        //~^ ERROR: Block-scoped variable 'foo' used before its declaration.
        await null
        promise
        foo
        //~^ ERROR: Block-scoped variable 'foo' used before its declaration.
    })()

    const foo = 1;
}

// #30907
function wrapI1() {
    const iter = (function* foo() {
        iter;
        yield 1;
    })();
}

function wrapI2() {
    const iter = (async function* foo() {
        iter;
        yield 1;
    })();
}

function foo18() {
    let a = (() => Enum.Yes)();
    //~^ ERROR: Enum 'Enum' used before its declaration.
    //~| ERROR: Enum 'Enum' used before its declaration.
    enum Enum {
        No = 0,
        Yes = 1,
    }
}

