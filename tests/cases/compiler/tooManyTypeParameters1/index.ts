// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/tooManyTypeParameters1.ts`, Apache-2.0 License

function f<T>() { }
f<string, string>();
//~^ ERROR: Expected 1 type arguments, but got 2.

var x = <T>() => {};
x<number,number>();
//~^ ERROR: Expected 1 type arguments, but got 2.

class C<T> {}
var c = new C<Date,Date>();
//~^ ERROR: Expected 1 type arguments, but got 2.

interface I<T> {}
var i: I<number,number>;
//~^ ERROR: Generic type 'I<T>' requires 1 type argument.
