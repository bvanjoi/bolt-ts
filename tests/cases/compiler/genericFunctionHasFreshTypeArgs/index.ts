// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericFunctionHasFreshTypeArgs.ts`, Apache-2.0 License

function f(p: <T>(x: T) => void) { };
f(x => f(y => x = y));