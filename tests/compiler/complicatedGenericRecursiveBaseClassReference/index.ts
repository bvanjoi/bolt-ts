// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/complicatedGenericRecursiveBaseClassReference.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class S18<B, A, C> extends S18<A[], { S19: A; (): A }[], C[]>
//~^ ERROR: 'S18' is referenced directly or indirectly in its own base expression.
{
}
(new S18(123)).S18 = 0;
//~^ ERROR: Expected 0 arguments, but got 1.
//~| ERROR: Property 'S18' does not exist on type 'S18<unknown, unknown, unknown>'.