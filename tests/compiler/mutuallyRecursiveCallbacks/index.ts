// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mutuallyRecursiveCallbacks.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Foo<T> { (bar: Bar<T>): void };
type Bar<T> = (foo: Foo<T>) => Foo<T>;
declare function foo<T>(bar: Bar<T>): void;
declare var bar: Bar<{}>;
bar = foo;
//~^ ERROR: Type '(bar: Bar) => void' is not assignable to type 'Bar'.
