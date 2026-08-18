// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/observableInferenceCanBeMade.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function asObservable(input) {
  return typeof input === 'string' ? of(input) : from(input);
}