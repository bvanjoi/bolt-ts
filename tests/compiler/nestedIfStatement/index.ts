// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/nestedIfStatement.ts`, Apache-2.0 License

//@compiler-options: target=es2015

if (0) {
} else if (1) {
} else if (2) { //~ERROR: This kind of expression is always truthy.
} else if (3) { //~ERROR: This kind of expression is always truthy.
} else {
}