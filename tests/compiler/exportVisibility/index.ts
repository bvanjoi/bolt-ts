// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/exportVisibility.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export class Foo {
}

export var foo = new Foo();

export function test(foo: Foo) {
    return true;
}