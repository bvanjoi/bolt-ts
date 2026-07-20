// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/circularReferenceInReturnType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

// inference fails for res1 and res2, but ideally should not
declare function fn1<T>(cb: () => T): string;
const res1 = fn1(() => res1);
//~^ ERROR: 'res1' implicitly has type 'any' because it does not have a type annotation and is referenced directly or indirectly in its own initializer.
//~| ERROR: Function implicitly has return type 'any' because it does not have a return type annotation and is referenced directly or indirectly in one of its return expressions.

declare function fn2<T>(): (cb: () => any) => (a: T) => void;
const res2 = fn2()(() => res2);

declare function fn3<T>(): <T2>(cb: (arg: T2) => any) => (a: T) => void;
const res3 = fn3()(() => res3);

// https://github.com/microsoft/TypeScript/issues/58616

function foo(arg: Parameters<typeof bar>[0]) {
    return arg;
}

function bar(arg: string) {
    return foo(arg);
}
