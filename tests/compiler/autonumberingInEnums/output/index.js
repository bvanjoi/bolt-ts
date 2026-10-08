// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/autonumberingInEnums.ts`, Apache-2.0 License
var Foo = {};
(function (Foo) {

  // should work fine
  Foo[Foo['a'] = 1] = 'a'
})(Foo);

(function (Foo) {

  Foo[Foo['b'] = 0] = 'b'
})(Foo);