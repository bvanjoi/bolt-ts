var M = {};
(function (M) {

  var MyEnum = {};
  (function (MyEnum) {
  
    MyEnum[MyEnum['BAR'] = 0] = 'BAR'
  })(MyEnum);
  M.MyEnum = MyEnum;
  
  var object2 = {
      foo: MyEnum.BAR    
  };
  M.object2 = object2
  
})(M);
var N = {};
(function (N) {

  var MyEnum = {};
  (function (MyEnum) {
  
    MyEnum[MyEnum['FOO'] = 0] = 'FOO'
  })(MyEnum);
  N.MyEnum = MyEnum;
  
  ;
  
  var object1 = {
      foo: MyEnum.FOO    
  };
  N.object1 = object1
  
})(N);
var broken = [N.object1, M.object2];