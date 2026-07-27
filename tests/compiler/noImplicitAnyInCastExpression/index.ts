// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyInCastExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

interface IFoo {
    a: number;
    b: string;
}

// Expr type not assignable to target type
(<IFoo>{ a: null });
//~^ ERROR: Property 'b' is missing.

// Expr type assignable to target type
(<IFoo>{ a: 2, b: undefined });
//~^ ERROR: Conversion of type '{ a: number; b: undefined; }' to type 'IFoo' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.

// Neither types is assignable to each other
(<IFoo>{ c: null });
//~^ ERROR: Property 'a' is missing.
//~| ERROR: Property 'b' is missing.
