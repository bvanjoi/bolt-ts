// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/destructuringWithNewExpression.ts`, Apache-2.0 License
class C {
  x = 0
}
var {x} = new C;