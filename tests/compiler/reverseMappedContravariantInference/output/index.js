// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reverseMappedContravariantInference.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
conforms({
  foo: (v) => (false)  
})({
  foo: 'hello'  
});