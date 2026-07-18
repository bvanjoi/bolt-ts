// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferredRestTypeFixedOnce.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function wrap<Args extends unknown[]>(_: (...args: Args) => void) {}
wrap(({ cancelable } = {}) => {});
