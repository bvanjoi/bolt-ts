// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/nonExportedElementsOfMergedModules.ts`, Apache-2.0 License

module One {
  enum A { X }
  module B {
      export var x;
  }
}

module One {
  enum A { Y }
  module B {
      export var y;
  }
  B.x;
  //~^ Error: Property 'x' does not exist on type 'typeof B'.
  B.y;
}
