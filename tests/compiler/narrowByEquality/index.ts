// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowByEquality.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

declare let x: number | string | boolean
declare let n: number;
declare let s: string;
declare let b: boolean;
declare let xUnknown: unknown;

if (x == n) {
    x;
}

if (x == s) {
    x;
}

if (x == b) {
    x;
}

if (x == 1) {
    x;
}

if (x == "") {
    x;
}

if (x == "foo") {
    x;
}

if (x == true) {
    x;
}

if (x == false) {
    x;
}

declare let xAndObj: number | string | boolean | object

if (xAndObj == {}) {
  //~^ ERROR: This condition will always return 'false' since JavaScript compares objects by reference, not value.
    xAndObj;
}

if (x == xAndObj) {
    x;
    xAndObj;
}

// Repro from #24991

function test(level: number | string):number {
    if (level == +level) {
        const q2: number = level; // error
        //~^ ERROR: Type 'number | string' is not assignable to type 'number'.
        return level;
        //~^ ERROR: Type 'number | string' is not assignable to type 'number'.
    }
    return 0;
}

// From issue #32798
if (xUnknown == null) {
    xUnknown;
} else {
    xUnknown
}

if (xUnknown != null) {
    xUnknown;
} else {
    xUnknown;
}

