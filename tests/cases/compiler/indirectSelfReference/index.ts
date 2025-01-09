// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/indirectSelfReference.ts`, Apache-2.0 License

class a extends b{ }
//~^ ERROR: Class 'b' used before its declaration.
//~| ERROR: 'a' is referenced directly or indirectly in its own base expression.
class b extends a{ }
//~^ ERROR: 'b' is referenced directly or indirectly in its own base expression.
