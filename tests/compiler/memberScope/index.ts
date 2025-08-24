// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/memberScope.ts`, Apache-2.0 License

module Salt {
  export class Pepper {}
  export module Basil { }
  var z = Basil.Pepper;
  //~^ ERROR: Cannot find name 'Basil'.
}

