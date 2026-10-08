// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/discriminantPropertyInference.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: noImplicitAny
//@compiler-options: strictNullChecks
//@run-fail
// simple inference
f({
  disc: true,
  cb: (s) => (parseInt(s))  
});
// simple inference
f({
  disc: false,
  cb: (n) => (n.toFixed())  
});
// simple inference when strict-null-checks are enabled
f({
  disc: undefined,
  cb: (n) => (n.toFixed())  
});
// requires checking type information since discriminator is missing from object
f({
  cb: (n) => (n.toFixed())  
});