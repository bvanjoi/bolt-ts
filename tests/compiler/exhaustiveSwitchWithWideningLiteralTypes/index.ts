// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/exhaustiveSwitchWithWideningLiteralTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

class A {
    readonly kind = "A"; // (property) A.kind: "A"
}

class B {
    readonly kind = "B"; // (property) B.kind: "B"
}

function f(value: A | B): number {
    switch(value.kind) {
        case "A": return 0;
        case "B": return 1;
    }
}