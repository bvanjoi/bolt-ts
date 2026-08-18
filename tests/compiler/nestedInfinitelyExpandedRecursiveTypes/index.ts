// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nestedInfinitelyExpandedRecursiveTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface F<T> {
      t: G<F<() => T>>;
}
interface G<U> {
      t: G<G<() => U>>;
}
 
var f: F<string>;
var g: G<string>;
f = g;
//~^ ERROR: Variable 'g' is used before being assigned.
g = f;