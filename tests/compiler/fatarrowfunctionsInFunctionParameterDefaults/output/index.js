// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/fatarrowfunctionsInFunctionParameterDefaults.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function fn(x = () => (this), y = x()) {
  return y;
}
fn.call(4);