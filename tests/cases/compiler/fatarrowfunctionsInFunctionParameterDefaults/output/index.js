// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/fatarrowfunctionsInFunctionParameterDefaults.ts`, Apache-2.0 License
function fn(x = () => this, y = x()) {
  // should be 4
  return y
}
fn.call(4);