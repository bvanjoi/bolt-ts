// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericDerivedTypeWithSpecializedBase2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A<T extends { length: number }> {
    x: T;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
}

class B<U> extends A<string> {
    y: U;
    //~^ ERROR: Property 'y' has no initializer and is not definitely assigned in the constructor.
}

declare var x: A<{ length: number; foo: number }>;
declare var y: B<number>;
x = y;  // error
//~^ ERROR: Type 'B<number>' is not assignable to type 'A<{ length: number; foo: number; }>'.

