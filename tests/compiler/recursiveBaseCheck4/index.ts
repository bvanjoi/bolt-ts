// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveBaseCheck4.ts`, Apache-2.0 License

class M<T> extends M<string> { }
//~^ ERROR: 'M' is referenced directly or indirectly in its own base expression.
(new M).blah;
//~^ ERROR: Property 'blah' does not exist on type 'M<unknown>'.