// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/argumentsAsPropertyName.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

type MyType = {
    arguments: Array<string>
}

declare function use(s: any);

function myFunction(myType: MyType) {
    for (let i = 0; i < 10; i++) {
        use(myType.arguments[i]);
        // create closure so that tsc will turn loop body into function
        const x = 5;
        [1, 2, 3].forEach(function(j) { use(x); })
    }
}