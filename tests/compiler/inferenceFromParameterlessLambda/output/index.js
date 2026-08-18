// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferenceFromParameterlessLambda.ts`, Apache-2.0 License
function foo(o, i) {}
foo((n) => (n.length), () => ('hi'));