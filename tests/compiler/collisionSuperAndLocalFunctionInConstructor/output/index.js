function _super() {}
class Foo {
  constructor() {function _super() {}}
}
class b extends Foo {
  constructor() {super();function _super() {}}
}
class c extends Foo {
  constructor() {super();var x = () => {
      function _super() {}
    };}
}