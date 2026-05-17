// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/enumAssignmentCompat4.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace M {
    export enum MyEnum {
        BAR
    }
    export var object2 = {
        foo: MyEnum.BAR
    };
}

namespace N {
    export enum MyEnum {
        FOO
    };
    export var object1 = {
        foo: MyEnum.FOO
    };
}

let broken = [
    N.object1,
    M.object2
];
