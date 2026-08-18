// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeGuardNarrowsIndexedAccessOfKnownProperty7.ts`, Apache-2.0 License
//@compiler-options: strict
var Foo = {};
(function (Foo) {

  var key = Symbol();
  Foo.key = key
  
})(Foo);
export class C {
  [Foo.key];
  constructor() {this[Foo.key] = 'hello';}
}