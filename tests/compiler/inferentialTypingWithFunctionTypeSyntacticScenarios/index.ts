// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferentialTypingWithFunctionTypeSyntacticScenarios.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowUnreachableCode

declare function map<T, U>(array: T, func: (x: T) => U): U;
declare function identity<V>(y: V): V;
var s: string;

// dotted name
var dottedIdentity = { x: identity };
s = map("", dottedIdentity.x);

// index expression
s = map("", dottedIdentity['x']);

// function call
s = map("", (() => identity)());

// construct
interface IdentityConstructor {
    new (): typeof identity;
}
var ic: IdentityConstructor;
s = map("", new ic());
//~^ ERROR: Variable 'ic' is used before being assigned.

// assignment
var t;
s = map("", t = identity);

// type assertion
s = map("", <typeof identity>identity);

// parenthesized expression
s = map("", (identity));

// comma
s = map("", ("", identity));