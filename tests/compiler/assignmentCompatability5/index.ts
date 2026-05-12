// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentCompatability5.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace __test1__ {
    export interface interfaceWithPublicAndOptional<T,U> { one: T; two?: U; };  var obj4: interfaceWithPublicAndOptional<number,string> = { one: 1 };;
    export var __val__obj4 = obj4;
}
namespace __test2__ {
    export                   interface interfaceOne<T> { one: T; };                var obj1: interfaceOne<number> = { one: 1 };;
    export var __val__obj1 = obj1;
}
__test2__.__val__obj1 = __test1__.__val__obj4