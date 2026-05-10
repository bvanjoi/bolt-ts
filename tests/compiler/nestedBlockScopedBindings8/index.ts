// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/nestedBlockScopedBindings8.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var x;
for (let x; false; ) {
    () => x;
}

var y;
for (let y; false; ) {
    y = 1;
}