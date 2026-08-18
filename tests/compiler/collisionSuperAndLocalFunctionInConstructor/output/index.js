// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionSuperAndLocalFunctionInConstructor.ts`, Apache-2.0 License
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