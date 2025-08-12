function doThing(x) {}
class A {
  static n
  p = doThing(A)
}
class B extends A {
  p1 = doThing(A)
  p2 = doThing(B)
}
doThing(B);