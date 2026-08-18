// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/freshLiteralInference.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var value = f1('1');
var x1 = value;
var obj2 = f2({
  value: '1'  
});
var x2 = obj2.value;
var obj3 = f3({
  value: '1'  
});
var x3 = obj3.value;