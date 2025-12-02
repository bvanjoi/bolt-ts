// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/emptyThenWithoutWarning.ts`, Apache-2.0 License

let a = 4;

if(a === 1 || a === 2 || a === 3) {
}
else {
    let message = "Ooops";
}