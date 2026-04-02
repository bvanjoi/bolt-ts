// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/templateExpressionNoInlininingOfConstantBindingWithInitializer.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type Params = {
  value?: string | number
}

function example(parameters: Params) {
  const { value = '123' } = parameters
  return `${value}` === '345'
}

function example2(parameters: Params) {
  const { value = '123' } = parameters
  const b = `${value}`;
  return b;
}
