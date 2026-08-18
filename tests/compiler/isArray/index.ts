// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/isArray.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var maybeArray: number | number[];


if (Array.isArray(maybeArray)) {
  //~^ ERROR: Variable 'maybeArray' is used before being assigned.
    maybeArray.length; // OK
}
else {
    maybeArray.toFixed(); // OK
  //~^ ERROR: Variable 'maybeArray' is used before being assigned.
  //~| ERROR: Property 'toFixed' does not exist on type 'number | number[]'.
}