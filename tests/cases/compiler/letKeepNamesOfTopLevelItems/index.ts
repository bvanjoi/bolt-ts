// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/letKeepNamesOfTopLevelItems.ts`, Apache-2.0 License

let x;
function foo() {
    let x;
}

module A {
    let x;
}