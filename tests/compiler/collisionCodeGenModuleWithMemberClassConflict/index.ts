// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionCodeGenModuleWithMemberClassConflict.ts`, Apache-2.0 License

//@compiler-options: target=es2015
namespace m1 {
    export class m1 {
    }
}
var foo = new m1.m1();

namespace m2 {
    export class m2 {
    }

    export class _m2 {
    }
}
var foo = new m2.m2();
var foo = new m2._m2();