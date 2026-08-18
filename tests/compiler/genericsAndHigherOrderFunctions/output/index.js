// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/genericsAndHigherOrderFunctions.ts`, Apache-2.0 License
var combine = (f) => ((g) => ((x) => (f(g(x)))));
var foo = (g) => ((h) => ((f) => (h(combine(f)(g)))));