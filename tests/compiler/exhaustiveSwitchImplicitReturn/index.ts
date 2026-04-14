// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/exhaustiveSwitchImplicitReturn.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitReturns

function foo1(bar: "a"): number {
    switch(bar) {
        case "a":
            return 1;
    }
}

function foo2(bar: "a"): number {
    switch(bar) {
        case "a":
            return 1;
    }

    let unusedVariable;
}

function foo3(bar: "a"): number {
    switch(bar) {
        case "a":
            return 1;
    }

    function neverCalled() {}
}

function foo4(bar: "a"): number {
    switch(bar) {
        case "a":
            return 1;
    }

    foo3(bar);
}

function foo5(bar: "a" | "b"): number { //~ERROR: Not all code paths return a value.
    switch(bar) {
        case "a":
            return 1;
    }
}

function foo6(bar: "a", a: boolean, b: boolean): number {
    if (a) {
        switch (bar) {
            case "a": return 1;
        }
    }
    else {
        switch (b) {
            case true: return -1;
            case false: return 0;
        }
    }
}
