// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferenceOptionalPropertiesToIndexSignatures.ts`, Apache-2.0 License
//@compiler-options: strict
//@compiler-options: target=esnext
//@run-fail




var a1 = foo(x1);
var a2 = foo(x2);
var a3 = foo(x3);
var a4 = foo(x4);
var param2 = Math.random() < 0.5 ? 'value2' : null;
var obj = {
  param1: 'value1',
  ...(param2 ? {
      param2    
  } : {})  
};
var query // string | number
= Object.entries(obj).// string | number | undefined
map(([k, v]// string | number
) => (`${k}=${encodeURIComponent(v// string | number
// Repro from #43045
)}`)).join('&');