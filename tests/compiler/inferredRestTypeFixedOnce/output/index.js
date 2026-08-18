// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferredRestTypeFixedOnce.ts`, Apache-2.0 License
function wrap(_) {}
wrap(({cancelable} = {}) => {});