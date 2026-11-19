// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/deepComparisons.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f1<T, K1 extends keyof T, K2 extends keyof T[K1]>() {
    let v1: Extract<T, string> = 0 as any as T;  // Error
    //~^ ERROR: Type 'T' is not assignable to type 'Extract<T, string>'.
    let v2: Extract<T[K1], string> = 0 as any as T[K1];  // Error
    //~^ ERROR: Type 'T[K1]' is not assignable to type 'Extract<T[K1], string>'.
    let v3: Extract<T[K1][K2], string> = 0 as any as T[K1][K2];  // No error
    //~^ ERROR: Type 'T[K1][K2]' is not assignable to type 'Extract<T[K1][K2], string>'.
}

type Foo<T> = { x: Foo<T> };
type Bar<T> = { x: Bar<T[]> };

function f2<U>() {
    let x: Foo<U> = 0 as any as Bar<U>;  // Error, excessive stack depth
}

type Foo1<T> = { x: Foo2<T> };
type Foo2<T> = { x: Foo1<T> };

function f3<U>() {
    let x: Foo1<U> = 0 as any as Bar<U>;  // No error!
}

// Repro from #46500

type F<T> = {} & (
    T extends [any, ...any[]]
        ? { [K in keyof T]?: F<T[K]> }
        : T extends any[]
            ? F<T[number]>[]
            : T extends { [K: string]: any }
                ? { [K in keyof T]?: F<T[K]> }
                : { x: string }
);

declare function f<T = any>(): F<T>;

function g() {
    return f() as F<any>;
}