// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/recursiveInference1.ts`, Apache-2.0 License

function fib(x:number) { return x <= 1 ? x : fib(x - 1) + fib(x - 2); }
var result = fib(5);
var result1: number = fib(5);


function fib2(x:number): number { return x <= 1 ? x : fib2(x - 1) + fib2(x - 2); }
var result2 = fib2(5);
var result21: number = fib2(5);