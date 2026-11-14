// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/argumentsSpreadRestIterables.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: noEmit
//@[target=ES5]     compiler-options: target=es5
//@[target=ESNext]  compiler-options: target=esnext

declare const itNum: Iterable<number>

;(function(...rest) {})(...itNum)
//~[target=ES5]^ Type 'Iterable<number, any, any>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
//~[target=ES5]| Type 'Iterable<number, any, any>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
;(function(a, ...rest) {})('', ...itNum)
//~[target=ES5]^ Type 'Iterable<number, any, any>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
//~[target=ES5]| Type 'Iterable<number, any, any>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
;(function(a, ...rest) {})('', true, ...itNum)
//~[target=ES5]^ Type 'Iterable<number, any, any>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
//~[target=ES5]| Type 'Iterable<number, any, any>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.

declare function fn1<const T extends readonly unknown[]>(...args: T): T;
const res1 = fn1(..."hello");
//~[target=ES5]^ Type '"hello"' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
//~[target=ES5]| Type '"hello"' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
const res2 = fn1(...itNum);
//~[target=ES5]^ Type 'Iterable<number, any, any>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
//~[target=ES5]| Type 'Iterable<number, any, any>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
const res3 = fn1(true, ..."hello");
//~[target=ES5]^ Type '"hello"' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
//~[target=ES5]| Type '"hello"' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
const res4 = fn1(true, ...itNum);
//~[target=ES5]^ Type 'Iterable<number, any, any>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
//~[target=ES5]| Type 'Iterable<number, any, any>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.

// repro from #52781
declare function foo<T extends unknown[]>(...args: T): T;
const p1 = foo(..."hello");
//~[target=ES5]^ Type '"hello"' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
//~[target=ES5]| Type '"hello"' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
const p2 = foo(...itNum);
//~[target=ES5]^ Type 'Iterable<number, any, any>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
//~[target=ES5]| Type 'Iterable<number, any, any>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
const p3 = foo(true, ..."hello");
//~[target=ES5]^ Type '"hello"' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
//~[target=ES5]| Type '"hello"' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
const p4 = foo(true, ...itNum);
//~[target=ES5]^ Type 'Iterable<number, any, any>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
//~[target=ES5]| Type 'Iterable<number, any, any>' can only be iterated through when using the '--downlevelIteration' flag or with a '--target' of 'es2015' or higher.
