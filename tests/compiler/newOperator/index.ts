// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/newOperator.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface ifc { }
// Attempting to 'new' an interface yields poor error
var i = new ifc();
//~^ ERROR: Cannot find name 'ifc'.

// Parens are optional
var x = new Date;
var y = new Date();

// Target is not a class or var, good error
var t1 = new 53();
//~^ ERROR: This expression is not constructable.
var t2 = new ''();
//~^ ERROR: This expression is not constructable.
new string;
//~^ ERROR: Cannot find name 'string'.

// Use in LHS of expression?
(new Date()).toString();

// Various spacing
var t3 = new string[]( );
//~^ ERROR: Cannot find name 'string'.
//~| ERROR: An element access expression should take an argument.
var t4 =
new
string
[
    ]
    (
        );
//~^^^^^ ERROR: Cannot find name 'string'.
//~^^^^ ERROR: An element access expression should take an argument.

// Unresolved symbol
var f = new q();
//~^ ERROR: Cannot find name 'q'.

// not legal
var t5 = new new Date;
//~^ ERROR: This expression is not constructable.

// Can be an expression
new String;

// Error on union
declare const union: { a: string } | { b: string }
new union;
//~^ ERROR: This expression is not constructable.

// Error on union with one constructor
declare const ctorUnion: { a: string } | (new (a: string) => void)
new ctorUnion("");
//~^ ERROR: This expression is not constructable.

// Error on union with incompatible constructors
declare const ctorUnion2: (new <T extends number>(a: T) => void) | (new <T>(a: string) => void)
new ctorUnion2("");
//~^ ERROR: This expression is not constructable.

namespace M {
    export class T {
        x: number;
        //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
    }
}

class S {
    public get xs(): M.T[] {
        return new M.T[];
        //~^ ERROR: Element implicitly has an 'any' type because expression of type 'error' can't be used to index type 'typeof T'.
        //~| ERROR: An element access expression should take an argument.
    }
}
