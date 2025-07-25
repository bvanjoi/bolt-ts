// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/callSignatureFunctionOverload.ts`, Apache-2.0 License

var foo: {
    (name: string): string;
    (name: 'order'): string;
    (name: 'content'): string;
    (name: 'done'): string;
}

var foo2: {
    (name: string): string;
    (name: 'order'): string;
    (name: 'order'): string;
    (name: 'done'): string;
    (name: 'bar'): number;
    (name: `foo`): boolean;
}

let a: string = foo2('bar')
//~^ ERROR: Type 'number' is not assignable to type 'string'.
let b: string = foo2('foo')
//~^ ERROR: Type 'boolean' is not assignable to type 'string'.
