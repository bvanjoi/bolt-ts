// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/callWithWrongNumberOfTypeArguments.ts`, Apache-2.0 License

function f<T, U>() { }

f<number>();
//~^ ERROR: Expected 2 type arguments, but got 1.
f<number, string>();
f<number, string, number>();
//~^ ERROR: Expected 2 type arguments, but got 3.