// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/parseArrowFunctionWithFunctionReturnType.ts`, Apache-2.0 License

const fn = <T>(): (() => T) => null as any;
