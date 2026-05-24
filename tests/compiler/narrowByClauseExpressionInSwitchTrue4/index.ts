// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowByClauseExpressionInSwitchTrue4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare const f: 'a' | 'b' | 'c';

switch (true) {
  case f === "a":
  default:
    f;
  case f === "b":
    f;
}
