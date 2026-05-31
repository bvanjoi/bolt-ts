var _super = 10;
class Foo {
  constructor() {var _super = 10;}
}
class b extends Foo {
  constructor() {super();var _super = 10;}
}
class c extends Foo {
  constructor() {super();var x = () => {
      var _super = 10;
    };}
}