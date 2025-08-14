// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisInModule.ts`, Apache-2.0 License

module myMod {
  var x;
  this.x = 5;
  //~^ ERROR: 'this' cannot be referenced in a module or namespace body.
}