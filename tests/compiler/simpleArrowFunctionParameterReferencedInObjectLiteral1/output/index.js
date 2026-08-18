// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/simpleArrowFunctionParameterReferencedInObjectLiteral1.ts`, Apache-2.0 License
[].map(() => ([].map((p) => (({
  X: p  
})))));