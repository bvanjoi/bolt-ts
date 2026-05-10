// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/inferPropertyWithContextSensitiveReturnStatement.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare function repro<T>(config: {
  params: T;
  callback: () => (params: T) => number;
}): void;

repro({
  params: 1,
  callback: () => { return a => a + 1 },
});
