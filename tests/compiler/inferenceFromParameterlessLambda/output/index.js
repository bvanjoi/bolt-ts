// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferenceFromParameterlessLambda.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function foo(o, i) {}
// Infer string from second argument because it isn't context sensitive
foo((n) => (n.length), () => ('hi'));