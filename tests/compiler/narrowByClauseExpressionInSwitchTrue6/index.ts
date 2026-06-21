// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowByClauseExpressionInSwitchTrue6.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

interface A {
    kind: "a";
    aProps: string;
}

interface B {
    kind: "b";
    bProps: string;
}

interface C {
    kind: "c";
    cProps: string;
}


type MyType = A | B | C;

function isA(x: MyType) {
    switch (true) {
        default:
            const never: never = x;
        case x.kind === "a":
            x.aProps;
            break;
        case x.kind === "b":
            x.bProps;
            break;
        case x.kind === "c":
            x.cProps;
            break;
    }

    switch (true) {
        default:
            const never: never = x;
        case x.kind === "a": {
            x.aProps;
            break;
        }
        case x.kind === "b": {
            x.bProps;
            break;
        }
        case x.kind === "c": {
            x.cProps;
            break;
        }
    }
    
    switch (true) {
        default:
            x.aProps;
            break;
        case x.kind === "b":
            x.bProps;
            break;
        case x.kind === "c":
            x.cProps;
            break;
    }

    switch (true) {
        default:
            const never: never = x;
        case x.kind === "a":
            x.aProps;
            // fallthrough
        case x.kind === "b":
            x.bProps;
            //~^ ERROR: Property 'bProps' does not exist on type 'A | B'.
            // fallthrough
        case x.kind === "c":
            x.cProps;
            //~^ ERROR: Property 'cProps' does not exist on type 'MyType'.
    }
}
