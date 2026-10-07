// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/genericFunctionsNotContextSensitive.ts`, Apache-2.0 License
//@compiler-options: strict
var f = (_) => (_);
var a = f((_) => ((_) => (({}))));
// <K extends string>(_: K) => <G>(_: G) => {}
var f0 = (_) => {};
var a0 = f0((_) => (({})));