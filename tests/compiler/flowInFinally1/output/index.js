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