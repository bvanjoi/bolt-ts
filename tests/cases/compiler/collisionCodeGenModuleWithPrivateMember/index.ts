// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/collisionCodeGenModuleWithPrivateMember.ts`, Apache-2.0 License

module m1 {
  class m1 {
  }
  var x = new m1();
  export class c1 {
  }
}
var foo = new m1.c1();
