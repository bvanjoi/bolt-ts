var MyEnumType = {};
(function (MyEnumType) {

  MyEnumType[MyEnumType['foo'] = 0] = 'foo'
  MyEnumType[MyEnumType['bar'] = 0] = 'bar'
})(MyEnumType);
var _arr = [{
  key: 'foo'  
}, {
  key: 'bar'  
}];
var enumValue = MyEnumType.foo;
var x = _arr.map((o) => (MyEnumType[o.key] === enumValue));