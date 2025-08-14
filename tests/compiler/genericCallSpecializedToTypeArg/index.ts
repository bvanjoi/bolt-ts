// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericCallSpecializedToTypeArg.ts`, Apache-2.0 License

function dupe<T>(x: T): T {
  return x;
}
function dupeAndGetDist<U>(x: U): U {
  var y = dupe(x); //<-- dupe has incorrect type here
  y.getDist();     //<-- this requires a missing constraint, but it's not caught
  //~^ ERROR: Property 'getDist' does not exist on type 'U'.
  return y;
}
