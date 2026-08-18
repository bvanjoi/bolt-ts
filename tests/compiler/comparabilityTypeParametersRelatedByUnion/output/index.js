// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/comparabilityTypeParametersRelatedByUnion.ts`, Apache-2.0 License
class C {
  constructor(x) {}
  good(y) {
    if (y === this.x) {}
    
  }
  bad(y) {
    if (y === this.x) {}
    
  }
}