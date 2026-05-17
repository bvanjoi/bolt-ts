// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/fatArrowfunctionAsType.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare var b: <T>(x: T) => void ;

var c: <T>(x: T) => void = function <T>(x: T) { return 42; }

b = c;
