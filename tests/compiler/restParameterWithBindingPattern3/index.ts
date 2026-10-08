// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/restParameterWithBindingPattern3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function a(...[a = 1, b = true]: string[]) { }
//~^ ERROR: Type 'number' is not assignable to type 'string'.
//~| ERROR: Type 'boolean' is not assignable to type 'string'.

function b(...[...foo = []]: string[]) { }
//~^ ERROR: A rest element cannot have an initializer.

function c(...{0: a, length, 3: d}: [boolean, string, number]) { }
//~^ ERROR: Tuple type '[boolean, string, number]' of length '3' has no element at index '3'.

function d(...[a, , , d]: [boolean, string, number]) { }
//~^ ERROR: Tuple type '[boolean, string, number]' of length '3' has no element at index '3'.

function e(...{0: a = 1, 1: b = true, ...rest: rest}: [boolean, string, number]) { }
//~^ ERROR: Type 'number' is not assignable to type 'boolean'.
//~| ERROR: Type 'boolean' is not assignable to type 'string'.
//~| ERROR: A rest element cannot have a property name.

