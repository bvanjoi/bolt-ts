// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/circularConstraintYieldsAppropriateError.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
// This should not be a circularity error. See
export function getPrismaClient(options) {
  class PrismaClient {
    self;
    constructor(options) {return (this.self = applyModelsAndClientExtensions(this));}
  }
  return PrismaClient;
}
export function applyModelsAndClientExtensions(client) {
  return client;
}