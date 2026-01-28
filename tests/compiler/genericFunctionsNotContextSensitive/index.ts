// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/genericFunctionsNotContextSensitive.ts`, Apache-2.0 License

//@compiler-options: strict

const f = <F extends (...args: any[]) => <G>(x: G) => void>(_: F): F => _;
const a = f(<K extends string>(_: K) => _ => ({}));  // <K extends string>(_: K) => <G>(_: G) => {}

const f0 = <F extends <G>(x:G) => void>(_: F) => {};
const a0 = f0(<K extends any>(_: K) => ({}))
