var MyEnum = {};
(function (MyEnum) {

  MyEnum[MyEnum['A'] = 0] = 'A'
  MyEnum[MyEnum['B'] = 0] = 'B'
  MyEnum[MyEnum['C'] = 0] = 'C'
})(MyEnum);
var MyStringEnum = {};
(function (MyStringEnum) {

  MyStringEnum[MyStringEnum['A'] = 'a'] = 'A'
  MyStringEnum[MyStringEnum['B'] = 'b'] = 'B'
  MyStringEnum[MyStringEnum['C'] = 'c'] = 'C'
})(MyStringEnum);
var MyStringEnumWithEmpty = {};
(function (MyStringEnumWithEmpty) {

  MyStringEnumWithEmpty[MyStringEnumWithEmpty['A'] = ''] = 'A'
  MyStringEnumWithEmpty[MyStringEnumWithEmpty['B'] = 'b'] = 'B'
  MyStringEnumWithEmpty[MyStringEnumWithEmpty['C'] = 'c'] = 'C'
})(MyStringEnumWithEmpty);
export function fn(optionalEnum) {
  return optionalEnum ?? MyEnum.A;
}
export function fn2(optionalEnum) {
  return optionalEnum || MyEnum.B;
}
export function fn3(optionalEnum) {
  return optionalEnum ?? MyEnum.A;
}
export function fn4(optionalEnum) {
  return optionalEnum || MyEnum.B;
}
export function fn5(optionalEnum) {
  return optionalEnum || MyStringEnum.B;
}
export function fn6(optionalEnum) {
  return optionalEnum || MyStringEnumWithEmpty.B;
}