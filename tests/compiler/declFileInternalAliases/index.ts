// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileInternalAliases.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

namespace m {
    export class c {
    }
}
namespace m1 {
    import x = m.c;
    export var d = new x(); // emit the type as m.c
}
namespace m2 {
    export import x = m.c;
    export var d = new x(); // emit the type as x
}