// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexerConstraints2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A { a: number; }
//~^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
class B extends A { b: number; }
//~^ ERROR: Property 'b' has no initializer and is not definitely assigned in the constructor.

// Inheritance
class F {
    [s: string]: B
}
class G extends F {
    [n: number]: A
    //~^ ERROR: 'number' index type 'A' is not assignable to 'string' index type 'B'.
}

// Other way
class H {
    [n: number]: A
}
class I extends H {
    [s: string]: B
    //~^ ERROR: 'number' index type 'A' is not assignable to 'string' index type 'B'.
}

// With hidden indexer
class J {
    [n: number]: {}
}

class K extends J {
    [n: number]: A;
    //~^ ERROR: 'number' index type 'A' is not assignable to 'string' index type 'B'.
    [s: string]: B;
}


type AliasedNumber = number;

interface L {
    [n: AliasedNumber]: A;
}

type AliasedString = string;

interface M {
    [s: AliasedString]: A;
}

type AliasedBoolean = boolean;

interface N {
    [b: AliasedBoolean]: A;
    //~^ ERROR: An index signature parameter type must be 'string', 'number', 'symbol', or a template literal type.
}

type IndexableUnion = "foo" | "bar";

interface O {
    [u: IndexableUnion]: A;
    //~^ ERROR: An index signature parameter type cannot be a literal type or generic type. Consider using a mapped object type instead.
}

type NonIndexableUnion = boolean | {};

interface P {
    [u: NonIndexableUnion]: A;
    //~^ ERROR: An index signature parameter type must be 'string', 'number', 'symbol', or a template literal type.
}

type NonIndexableUnion2 = string | number;

interface Q {
    [u: NonIndexableUnion2]: A;
}

type NonIndexableUnion3 = "foo" | 42;

interface R {
    [u: NonIndexableUnion3]: A;
    //~^ ERROR: An index signature parameter type cannot be a literal type or generic type. Consider using a mapped object type instead.
}

interface S {
    [u: "foo" | "bar"]: A;
    //~^ ERROR: An index signature parameter type cannot be a literal type or generic type. Consider using a mapped object type instead.
}

type Key = string;
interface T {
    [key: Key]
    //~^ ERROR: An index signature must have a type annotation.
}
