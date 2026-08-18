// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/unionTypeParameterInference.ts`, Apache-2.0 License
function unlift(value) {
  return lift(value).prop;
}