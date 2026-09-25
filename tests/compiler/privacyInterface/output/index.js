var m1 = {};
(function (m1) {

  class C1_public {
    f1() {}
  }
  m1.C1_public = C1_public;
  
  class C2_private {}
  
})(m1);
var m2 = {};
(function (m2) {

  class C1_public {
    f1() {}
  }
  m2.C1_public = C1_public;
  
  class C2_private {}
  
})(m2);
export class C5_public {
  f1() {}
}
class C6_private {}

