// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/discriminantPropertyInference.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: noImplicitAny
//@compiler-options: strictNullChecks
f({
  disc: true,
  cb: (s) => (parseInt(s))  
});
f({
  disc: false,
  cb: (n) => (n.toFixed())  
});
f({
  disc: undefined,
  cb: (n) => (n.toFixed())  
});
f({
  cb: (n) => (n.toFixed())  
});