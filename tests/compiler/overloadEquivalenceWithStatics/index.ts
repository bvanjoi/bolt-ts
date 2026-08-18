// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadEquivalenceWithStatics.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A1<T> {
static B<S>(v: A1<S>): A1<S>; // 1 
static B<S>(v: S): A1<S>; // 2 : Error Duplicate signature
static B<S>(v: any): A1<S> {
return null;
//~^ ERROR: Type 'null' is not assignable to type 'A1<S>'.
}
}