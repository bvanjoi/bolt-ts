// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/missingTypeArguments2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A<T> { }

var x: () => A;
//~^ ERROR: Generic type 'A<T>' requires 1 type argument.
(a: A) => { };
//~^ ERROR: Generic type 'A<T>' requires 1 type argument.
var y: A<A>;
//~^ ERROR: Generic type 'A<T>' requires 1 type argument.
(): A => null;
//~^ ERROR: Generic type 'A<T>' requires 1 type argument.
