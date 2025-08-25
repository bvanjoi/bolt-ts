// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/checkTypePredicateForRedundantProperties.ts`, Apache-2.0 License

function addProp2(x: any): x is { a: string; a: string; } {
  //~^ ERROR: Duplicate identifier 'a'.
    return true;
}
