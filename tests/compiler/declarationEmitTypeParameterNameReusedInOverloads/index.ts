// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitTypeParameterNameReusedInOverloads.ts`, Apache-2.0 License

//@compiler-options: module=commonjs
//@compiler-options: target=es2015
//@compiler-options: declaration
//@compiler-options: skipLibCheck=false

export class Base { foo: string; }
//~^ ERROR: Property 'foo' has no initializer and is not definitely assigned in the constructor.
export class Derived extends Base { bar: string; }
//~^ ERROR: Property 'bar' has no initializer and is not definitely assigned in the constructor.
export class Derived2 extends Derived { baz: string; }
//~^ ERROR: Property 'baz' has no initializer and is not definitely assigned in the constructor.

export type Foo = {
    new (x: {
        new <T extends Derived>(a: T): T;
        new <T extends Base>(a: T): T;
    }): any[];
    new (x: {
        new <T extends Derived2>(a: T): T;
            new <T extends Base>(a: T): T;
    }): any[];
}
