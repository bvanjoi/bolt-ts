// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentsAfterFunctionExpression1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: removeComments=false
var v = {
  f: (a) => (0/*t1*/ ),
  g: ((a) => (0))/*t2*/ ,
  h: ((a) => (0/*t3*/ ))  
};