// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/mappedTypePartialNonHomomorphicBaseConstraint.ts`, Apache-2.0 License
class Model {
  getErrors() {
    return {
          base: ['some base error']      
    };
  }
}