class A {
  constructor(props) {
    this.props = props}
  doSomething() {
    this.props.foo && this.props.foo();
  }
}
class Monkey {
  constructor(a) {
    this.a = a}
  render() {
    if (this.a) {
      this.a.color;
    }
    
  }
}
class BigMonkey extends Monkey {
  render() {
    if (this.a) {
      this.a.color;
    }
    
  }
}
function f1(obj) {
  if (obj) {
    obj.x;
    obj['x'];
    obj();
  }
  
}
function f2(obj) {
  if (obj) {
    obj.x;
    obj['x'];
    obj();
  }
  
}
function f3(obj) {
  if (obj) {
    obj.x;
    obj['x'];
    obj();
  }
  
}
function f4(obj, x) {
  if (obj) {
    obj[x].length;
  }
  
}
function f5(obj, key) {
  if (obj) {
    obj[key];
  }
  
}
function f6(a) {
  if (typeof a !== 'string') {
    new a();
  }
  
}