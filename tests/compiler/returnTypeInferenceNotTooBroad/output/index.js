// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/returnTypeInferenceNotTooBroad.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@run-fail
var y = sepsis({
  low: 1,
  sign: {
      kind: 'a',
    a: 3    
  }  
});
var // $ExpectType { kind: "a"; a: 3; }
yun = unwrap(y);
var yone // $ExpectType { kind: "a"; a: 3; }
= unwrap(sepsis({
  low: 1,
  sign: {
      kind: 'a',
    a: 3    
  }  
}));