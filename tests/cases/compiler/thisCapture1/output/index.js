// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisExpressionInIndexExpression.ts`, Apache-2.0 License
class X {
  y = 0
  getSettings(keys) {
    var ret;
    return ret.always(() => {
      this.y = 0;
    }).promise()
  }
}