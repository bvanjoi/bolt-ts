class A {}
A.hasOwnProperty("foo")
class B {
  constructor() {}
}
B.hasOwnProperty("foo")
var a = A.hasOwnProperty("foo")
var b = B.hasOwnProperty("foo")