// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/recursiveBaseCheck6.ts`, Apache-2.0 License

class S18<A> extends S18<{ S19: A; }>{ }
//~^ ERROR: 'S18' is referenced directly or indirectly in its own base expression.
(new S18()).blah;
//~^ ERROR: Property 'blah' does not exist on type 'S18<unknown>'.