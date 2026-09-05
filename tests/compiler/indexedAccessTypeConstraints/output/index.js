class Parent {
  constructor(data) {}
  getData() {
    return this.data;
  }
}
export class Foo extends Parent {
  getContent() {
    return this.getData().get('content');
  }
}
export class Bar extends Parent {
  getContent() {
    return this.getData().get('content');
  }
}
function foo(x, y) {
  x = y;
}