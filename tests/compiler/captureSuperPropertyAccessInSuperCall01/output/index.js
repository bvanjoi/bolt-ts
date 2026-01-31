class A {
  constructor(f) {}
  blah() {
    return ''
  }
}
class B extends A {
  constructor() {super(() => (super.blah()));}
}