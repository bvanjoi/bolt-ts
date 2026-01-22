// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/genericFunctionsNotContextSensitive.ts`, Apache-2.0 License

//@compiler-options: strict

// TODO:
// const f = <F extends (...args: any[]) => <G>(x: G) => void>(_: F): F => _;
// const a = f(<K extends string>(_: K) => _ => ({}));  // <K extends string>(_: K) => <G>(_: G) => {}

// ty_of_f_args[0]
// 31137 -> 80
// ty_of_ty_param(F)
// 31133 -> 77
// ty_of_constraint_of_ty_param(F)
// 31135 -> 79

const f0 = <F extends <G>(x:G) => void>(_: F) => {};
const a0 = f0(<K extends any>(_: K) => ({}))
