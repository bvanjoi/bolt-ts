// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleNoneErrors.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: module=none

export class Foo {
    foo: string;
    //~^ ERROR: Property 'foo' has no initializer and is not definitely assigned in the constructor.
}