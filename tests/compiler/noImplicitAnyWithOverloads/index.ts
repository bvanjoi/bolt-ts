// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyWithOverloads.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

interface A {
    foo;
    //~^ ERROR: Member 'foo' implicitly has an 'any' type.
}
interface B { }

function callb(lam: (l: A) => void);
//~^ ERROR: 'callb', which lacks return-type annotation, implicitly has an 'any' return type.
function callb(lam: (n: B) => void);
//~^ ERROR: 'callb', which lacks return-type annotation, implicitly has an 'any' return type.
function callb(a) { }
//~^ ERROR: Parameter 'a' implicitly has an 'any' type.
callb((a) => { a.foo; }); // error, chose first overload