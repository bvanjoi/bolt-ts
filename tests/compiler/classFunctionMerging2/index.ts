// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classFunctionMerging2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare abstract class A {
    constructor(p: number);
    a: number;
}

declare function B(p: string): B;
declare class B extends A {
    constructor(p: string);
    b: number;
}

let b = new B("Hey")
console.log(b.a)

let d = B(42)
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.