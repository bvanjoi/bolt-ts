// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/recursiveBaseCheck3.ts`, Apache-2.0 License

class A<T> extends C<T> { }
//~^ ERROR: Class 'C' used before its declaration.
//~| ERROR: 'A' is referenced directly or indirectly in its own base expression.
class C<T> extends A<T> { }
//~^ ERROR: 'C' is referenced directly or indirectly in its own base expression.

(new C).blah;
//~^ ERROR: Property 'blah' does not exist on type 'C<unknown>'.