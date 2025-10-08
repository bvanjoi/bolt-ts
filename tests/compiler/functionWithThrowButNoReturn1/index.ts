// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/functionWithThrowButNoReturn1.ts`, Apache-2.0 License

function fn(): number {
  throw new Error('NYI');
  var t;
}
