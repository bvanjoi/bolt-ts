// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/library_ArraySlice.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// Array.prototype.slice can have zero, one, or two arguments
Array.prototype.slice();
Array.prototype.slice(0);
Array.prototype.slice(0, 1);