// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/genericFunctionsNotContextSensitive.ts`, Apache-2.0 License
var f = (_) => (_);
var a = f((_) => ((_) => (({}))));
var f0 = (_) => {};
var a0 = f0((_) => (({})));