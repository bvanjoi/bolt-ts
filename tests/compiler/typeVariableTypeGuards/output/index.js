// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeVariableTypeGuards.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
// Repro from #14091
class A {
  constructor(props) {
    this.props = props}
  doSomething() {
    this.props.foo && this.props.// Repro from #14415
    foo();
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
}// Another repro

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
// https://github.com/microsoft/TypeScript/issues/57381
function f6(a) {
  if (typeof a !== 'string') {
    new a();
  }
  
}