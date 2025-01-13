// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/staticsNotInScopeInClodule.ts`, Apache-2.0 License

class Clod {
  static x = 10;
}

module Clod {
  var p = x; // x isn't in scope here
  //~^ ERROR: Cannot find name 'x'.
}