// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/qualifiedName_entity-name-resolution-does-not-affect-class-heritage.ts`, Apache-2.0 License

module Alpha {
  export var x = 100;
}

class Beta extends Alpha.x {
//~^ ERROR: Type 'number' is not a constructor function type.
}