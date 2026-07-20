// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/circularAccessorAnnotations.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration

declare const c1: {
    get foo(): typeof c1.foo;
    //~^ ERROR: 'foo' is referenced directly or indirectly in its own type annotation.
}

declare const c2: {
    set foo(value: typeof c2.foo);
    //~^ ERROR: 'foo' is referenced directly or indirectly in its own type annotation.
}

declare const c3: {
    get foo(): string;
    set foo(value: typeof c3.foo);
}

type T1 = {
    get foo(): T1["foo"];
    //~^ ERROR: 'foo' is referenced directly or indirectly in its own type annotation.
}

type T2 = {
    set foo(value: T2["foo"]);
    //~^ ERROR: 'foo' is referenced directly or indirectly in its own type annotation.
}

type T3 = {
    get foo(): string;
    set foo(value: T3["foo"]);
}
