// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/collisionCodeGenModuleWithMemberInterfaceConflict.ts`, Apache-2.0 License

module m1 {
  export interface m1 {
  }
  export class m2 implements m1 {
  }
}
var foo = new m1.m2();