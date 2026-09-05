class A {
  constructor(stringOrUndefined) {
    this.stringOrUndefined = stringOrUndefined}
}
class B {
  constructor(str) {
    this.str = str}
}
var a = new A('123');
if (a instanceof A && a.stringOrUndefined) {
  new B(a.stringOrUndefined);
}

if (a.stringOrUndefined && a instanceof A) {
  new B(a.stringOrUndefined);
}

if (a instanceof A) {
  if (a.stringOrUndefined) {
    new B(a.stringOrUndefined);
  }
  
}

if (a.stringOrUndefined) {
  if (a instanceof A) {
    new B(a.stringOrUndefined);
  }
  
}
