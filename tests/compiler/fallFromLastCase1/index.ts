// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/fallFromLastCase1.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: strict=false
//@compiler-options: noFallthroughCasesInSwitch

declare function use(a: string);

function foo1(a: number) {
    switch (a) {
        case 1:
            use("1");
            break;
        case 2:
            use("2");
    }
}


function foo2(a: number) {
    switch (a) {
        case 1:
            use("1");
            break;
        default:
            use("2");
    }
}