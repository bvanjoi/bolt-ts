// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/nestedBlockScopedBindings14.ts`, Apache-2.0 License

var x;
for (; false;) {
    let x;
    () => x;
}

var y;
for (; false;) {
    let y;
    y = 1;
}