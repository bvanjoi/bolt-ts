// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classFunctionMerging.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare function Foo (x: number): Foo.Inst;
declare class Foo {
    constructor(x: string);
}
declare namespace Foo {
    export type Inst = number;
}

const a = new Foo("");
const b = Foo(12);

const a0: string = new Foo("");
//~^ ERROR: Type 'Foo' is not assignable to type 'string'.
const b0: string = Foo(12);
//~^ ERROR: Type 'number' is not assignable to type 'string'.
