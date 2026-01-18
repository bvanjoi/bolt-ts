// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/controlFlowDestructuringParameters.ts`, Apache-2.0 License

//@compiler-options: strictNullChecks

[{ x: 1 }].map(
  ({ x }) => x
);