// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/freshLiteralInference.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@run-fail
var value = f1('1');
// regular "1"
var x1 = value;// regular "1"

var obj2 = f2({
  value: '1'  
});
// { value: regular "1" }
var x2 = obj2.value;// regular "1"

var obj3 = f3({
  value: '1'  
});
// before: { value: fresh "1" }
var x3 = obj3.value;