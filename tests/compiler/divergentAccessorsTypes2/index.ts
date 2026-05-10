// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/divergentAccessorsTypes2.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: strict

class Test1<T> {
    get foo(): T { return null as any }
    set foo(s: T | undefined ) {
    }
}

const s = new Test1<string>();
s.foo = undefined;
s.foo = "hello";
s.foo = 42;
//~^ ERROR: Type 'number' is not assignable to type 'undefined | string'.
