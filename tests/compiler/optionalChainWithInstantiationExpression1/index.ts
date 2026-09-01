// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalChainWithInstantiationExpression1.ts`, Apache-2.0 License

//@[target=es2019]  compiler-options: target=es2019
//@[target=es2020]  compiler-options: target=es2020

declare namespace A {
    export class b<T> {
        static d: number;
        constructor(x: T);
    }
}

type c = unknown;

declare const a: typeof A | undefined;

a?.b<c>.d;
//~^ ERROR: An instantiation expression cannot be followed by a property access.

a?.b.d;
a?.['b']<c>.d
//~^ ERROR: An instantiation expression cannot be followed by a property access.
