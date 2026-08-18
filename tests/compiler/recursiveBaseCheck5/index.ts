// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveBaseCheck5.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I1<T> extends I2<string> { }
//~^ ERROR: Type 'I1<T>' recursively references itself as a base type.
interface I2<T> extends I1<T> { }
//~^ ERROR: Type 'I2<T>' recursively references itself as a base type.
class X<T, U> implements I2<T> { }
(new X).blah;
//~^ ERROR: Property 'blah' does not exist on type 'X<unknown, unknown>'