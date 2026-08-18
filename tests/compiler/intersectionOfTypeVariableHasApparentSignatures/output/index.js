// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/intersectionOfTypeVariableHasApparentSignatures.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
//@compiler-options: noImplicitAny
f({
  props: {
      children: (({x}) => {})    
  }  
});