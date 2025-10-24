// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/controlFlowWithIncompleteTypes.ts`, Apache-2.0 License

declare var cond: boolean;

function foo1() {
    let x: string | number | boolean = 0;
    while (cond) {
        if (typeof x === "string") {
            x = x.slice();
        }
        else {
            x = "abc";
        }
    }
}

function foo2() {
    let x: string | number | boolean = 0;
    while (cond) {
        if (typeof x === "number") {
            x = "abc";
        }
        else {
            x = x.slice();
        }
    }
}