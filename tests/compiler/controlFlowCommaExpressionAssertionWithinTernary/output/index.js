// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowCommaExpressionAssertionWithinTernary.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function foo2(param) {
  var val = param !== undefined;
  return val ? (assert(param !== undefined) , param) : null;
}