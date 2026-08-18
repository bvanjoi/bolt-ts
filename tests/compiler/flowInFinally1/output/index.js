// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/flowInFinally1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class A {
  constructor() {}
  method() {}
}
var a = null;
try {
  a = new A();
}finally {
  if (a) {
    a.method();
  }
  
}