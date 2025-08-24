// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisExpressionInIndexExpression.ts`, Apache-2.0 License

class X {
  private y = 0;
  public getSettings(keys: string[]): any {
      var ret: any;
      return ret.always(() => {
          this.y = 0;
      }).promise();
  }
}