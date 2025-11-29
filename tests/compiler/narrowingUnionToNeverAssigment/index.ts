// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/narrowingUnionToNeverAssigment.ts`, Apache-2.0 License

//@compiler-options: strict

type Variants = "a" | "b" | "c" | "d";

function fx1(x: Variants) {
    if (x === "a" || x === "b") {        
    }
    else {
        const y: never = x;
        //~^ ERROR: Type 'string' is not assignable to type 'never'.
        // TODO: replace `string` with `'c' | 'd'`
    }
}

function fx2(x: Variants) {
    if (x === "a" || x === "b" || x === "c" || x === "d") {
    }
    else {
        const y: never = x;
    }
}

function fx3(x: 'a') {
    if (x === "a") {
    }
    else {
        const y: never = x;
    }
}

function fx4(x: 'a' | 'b') {
    if (x === "a" || x === "b") {
    }
    else {
        const y: never = x;
    }
}

function fx5(x: 'a' | 'b' | 'c') {
    if (x === "a" || x === "b" || x === "c") {
    }
    else {
        const y: never = x;
    }
}