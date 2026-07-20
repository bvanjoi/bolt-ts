// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/privacyCheckTypeOfInvisibleModuleNoError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

namespace Outer {
    namespace Inner {
        export var m: number;
    }

    export var f: typeof Inner; // Since we dont unwind inner any more, it is error here
}
