// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentCompatability40.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace __test1__ {
    export interface interfaceWithPublicAndOptional<T,U> { one: T; two?: U; };  var obj4: interfaceWithPublicAndOptional<number,string> = { one: 1 };;
    export var __val__obj4 = obj4;
}
namespace __test2__ {
    export           class classWithPrivate<T> { constructor(private one: T) {} }                       var x5 = new classWithPrivate(1);;
    export var __val__x5 = x5;
}
__test2__.__val__x5 = __test1__.__val__obj4
//~^ ERROR: Type '__test1__.interfaceWithPublicAndOptional<number, string>' is not assignable to type '__test2__.classWithPrivate<number>'.