// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/twiceNestedKeyofIndexInference.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@run-fail
var state = {
  a: {
      b: '',
    c: 0    
  },
  d: false  
};
var newState = set(state, ['a', 'b'], 'why');