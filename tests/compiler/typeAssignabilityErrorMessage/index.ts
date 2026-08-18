// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeAssignabilityErrorMessage.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: target=es2020
//@compiler-options: noEmit

// Example: different error code altogether

interface ThroughStream {
    a: string;
}
interface ReadStream {
    f: string;
    g: number;
    h: boolean;
    i: BigInt;
    j: symbol;
}
function foo(): ReadStream {
    return undefined as any as ThroughStream;
    //~^ ERROR: Type 'ThroughStream' is missing the following properties from type 'ReadStream': f, g, and 3 more.
}
function bar(): ReadStream {
    return undefined as any as ThroughStream;
    //~^ ERROR: Type 'ThroughStream' is missing the following properties from type 'ReadStream': f, g, and 3 more.
}

// Example: different elaboration

type Wrap = {
    someProp: Bar<number>;
}
type OtherWrap = {
    someProp: Foo<string>;
}
type Foo<T> = {
    foo: { what: T };
}
type Bar<T> = {
    foo: { what: T };
} | boolean;

function fun(param: Wrap): void {}

declare let fooStr: Foo<string>;
declare let otherWrap: OtherWrap;

let a: Bar<number> = fooStr;
//~^ ERROR: Type 'Foo<string>' is not assignable to type 'Bar<number>'.

fun(otherWrap);
//~^ ERROR: Argument of type 'OtherWrap' is not assignable to parameter of type 'Wrap'.