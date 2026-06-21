// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowArrays.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
//@compiler-options: noImplicitAny
//@run-fail

declare function cond(): boolean;

function f1() {
    let x = [];
    x[0] = 5;
    x[1] = "hello";
    x[2] = true;
    return x;  // (string | number | boolean)[]
}

const a: number[] = f1();
//~^ ERROR: Type '(false | true | number | string)[]' is not assignable to type 'number[]'.

function f2() {
    let x = [];
    x.push(5);
    x.push("hello");
    x.push(true);
    return x;  // (string | number | boolean)[]
}

function f3() {
    let x;
    x = [];
    x.push(5, "hello");
    return x;  // (string | number)[]
}

function f4() {
    let x = [];
    if (cond()) {
        x.push(5);
    }
    else {
        x.push("hello");
    }
    return x;  // (string | number)[]
}

function f5() {
    let x;
    if (cond()) {
        x = [];
        x.push(5);
    }
    else {
        x = [];
        x.push("hello");
    }
    return x;  // (string | number)[]
}

function f6() {
    let x;
    if (cond()) {
        x = 5;
    }
    else {
        x = [];
        x.push("hello");
    }
    return x;  // number | string[]
}

function f7() {
    let x = null;
    if (cond()) {
        x = [];
        while (cond()) {
            x.push("hello");
        }
    }
    return x;  // string[] | null
}

function f8() {
    let x = [];
    x.push(5);
    if (cond()) return x;  // number[]
    x.push("hello");
    if (cond()) return x;  // (string | number)[]
    x.push(true);
    return x;  // (string | number | boolean)[]
}

function f9() {
    let x = [];
    if (cond()) {
        x.push(5);
        return x;  // number[]
    }
    else {
        x.push("hello");
        return x;  // string[]
    }
}

function f10() {
    let x = [];
    if (cond()) {
        x.push(true);
        x;  // boolean[]
    }
    else {
        x.push(5);
        x;  // number[]
        while (cond()) {
            x.push("hello");
        }
        x;  // (string | number)[]
    }
    x.push(99);
    return x;  // (string | number | boolean)[]
}

function f11() {
    let x = [];
    if (x.length === 0) {  // x.length ok on implicit any[]
        x.push("hello");
    }
    return x;
}

function f12() {
    let x;
    x = [];
    if (x.length === 0) {  // x.length ok on implicit any[]
        x.push("hello");
    }
    return x;
}

function f13() {
    var x = [];
    x.push(5);
    x.push("hello");
    x.push(true);
    return x;  // (string | number | boolean)[]
}

function f14() {
    const x = [];
    x.push(5);
    x.push("hello");
    x.push(true);
    return x;  // (string | number | boolean)[]
}

function f15() {
    let x = [];
    while (cond()) {
        while (cond()) {}
        x.push("hello");
    }
    return x;  // string[]
}

function f16() {
    let x;
    let y;
    (x = [], x).push(5);
    (x.push("hello"), x).push(true);
    ((x))[3] = { a: 1 };
    return x;  // (string | number | boolean | { a: number })[]
}

function f17() {
    let x = [];
    x.unshift(5);
    x.unshift("hello");
    x.unshift(true);
    return x;  // (string | number | boolean)[]
}

function f18() {
    let x = [];
    x.push(5);
    x.unshift("hello");
    x[2] = true;
    return x;  // (string | number | boolean)[]
}

// Repro from #39470

declare function foo(arg: { val: number }[]): void;

let arr = []
arr.push({ val: 1, bar: 2 });
foo(arr);
