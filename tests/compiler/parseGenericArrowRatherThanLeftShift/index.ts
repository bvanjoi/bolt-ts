// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parseGenericArrowRatherThanLeftShift.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type Bar = ReturnType<<T>(x: T) => number>;
declare const a: Bar;

function foo<T>(_x: T) {}
const b = foo<<T>(x: T) => number>(() => 1);
