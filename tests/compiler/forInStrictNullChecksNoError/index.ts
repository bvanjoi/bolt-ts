// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/forInStrictNullChecksNoError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

function f(x: { [key: string]: number; } | null | undefined) {
    for (const key in x) {  // 1
        console.log(x[key]);  // 2
    }
    x["no"]; // should still error
    //~^ ERROR: 'x' is possibly 'undefined' or 'null'.
}