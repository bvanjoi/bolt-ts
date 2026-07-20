// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/checkInfiniteExpansionTermination.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface IObservable<T> {
  n: IObservable<T[]>; // Needed, must be T[]
}

// Needed
interface ISubject<T> extends IObservable<T> { }

interface Foo { x }
interface Bar { y }

var values: IObservable<Foo>;
var values2: ISubject<Bar>;
values = values2;
