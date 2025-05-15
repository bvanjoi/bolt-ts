// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/undefinedArgumentInference.ts`, Apache-2.0 License
function foo1(f1) {
  return undefined
}
var z1 = foo1({x: undefined,
y: undefined});