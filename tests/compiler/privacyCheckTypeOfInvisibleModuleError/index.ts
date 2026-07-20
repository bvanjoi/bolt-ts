// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/privacyCheckTypeOfInvisibleModuleError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

namespace Outer {
    namespace Inner {
        export var m: typeof Inner;
    }

    export var f: typeof Inner;
}
