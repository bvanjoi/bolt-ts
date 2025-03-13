// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/collisionCodeGenModuleWithMemberVariable.ts`, Apache-2.0 License

module m1 {
  export var m1 = 10;
  var b = m1;
}
var foo = m1.m1;