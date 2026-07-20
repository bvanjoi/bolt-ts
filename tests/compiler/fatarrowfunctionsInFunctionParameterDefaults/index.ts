// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/fatarrowfunctionsInFunctionParameterDefaults.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function fn(x = () => this, y = x()) {

  // should be 4
  return y;

}

fn.call(4); // Should be 4
