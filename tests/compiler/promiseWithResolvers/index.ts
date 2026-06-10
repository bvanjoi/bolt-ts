// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/promiseWithResolvers.ts`, Apache-2.0 License

//@compiler-options: target=esnext

type T = {};
const { promise, resolve, reject } = Promise.withResolvers<T>();
