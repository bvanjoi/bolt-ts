// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/indirectSelfReferenceGeneric.ts`, Apache-2.0 License

class a<T> extends b<T> { }
//~^ ERROR: Class 'b' used before its declaration.
//~| ERROR: 'a' is referenced directly or indirectly in its own base expression.
class b<T> extends a<T> { }
//~^ ERROR: 'b' is referenced directly or indirectly in its own base expression.
