// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLiteralsAgainstUnionsOfArrays01.ts`, Apache-2.0 License
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