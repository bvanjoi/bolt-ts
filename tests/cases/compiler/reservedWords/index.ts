// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/reservedWords.ts`, Apache-2.0 License

var obj = {
    if: 0,
    debugger: 2,
    break: 3,
    function: 4
}

//This compiles.

var obj2 = {
    if: 0,
    while: 1,
    debugger: 2,
    break: 3,
    function: 4
}
