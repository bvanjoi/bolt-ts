// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/controlFlowManyCallExpressionStatementsPerf.ts`, Apache-2.0 License

//@compiler-options: strict

function test(x: boolean): boolean { return x; }

let state = true;

if (state) {
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
  test(state as any && state);
}
