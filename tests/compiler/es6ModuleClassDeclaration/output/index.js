export class c {
  constructor() {}
  x = 10;
  y = 30;
  static k = 20;
  static l = 30;
  method1() {}
  method2() {}
  static method3() {}
  static method4() {}
}
class c2 {
  constructor() {}
  x = 10;
  y = 30;
  static k = 20;
  static l = 30;
  method1() {}
  method2() {}
  static method3() {}
  static method4() {}
}
new c();
new c2();
var m1 = {};
(function (m1) {

  class c3 {
    constructor() {}
    x = 10;
    y = 30;
    static k = 20;
    static l = 30;
    method1() {}
    method2() {}
    static method3() {}
    static method4() {}
  }
  m1.c3 = c3;
  
  class c4 {
    constructor() {}
    x = 10;
    y = 30;
    static k = 20;
    static l = 30;
    method1() {}
    method2() {}
    static method3() {}
    static method4() {}
  }
  
  new c();
  
  new c2();
  
  new c3();
  
  new c4();
  
})(m1);
var m2 = {};
(function (m2) {

  class c3 {
    constructor() {}
    x = 10;
    y = 30;
    static k = 20;
    static l = 30;
    method1() {}
    method2() {}
    static method3() {}
    static method4() {}
  }
  m2.c3 = c3;
  
  class c4 {
    constructor() {}
    x = 10;
    y = 30;
    static k = 20;
    static l = 30;
    method1() {}
    method2() {}
    static method3() {}
    static method4() {}
  }
  
  new c();
  
  new c2();
  
  new c3();
  
  new c4();
  
  new m1.c3();
  
})(m2);