// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionExportsRequireAndInternalModuleAliasInGlobalFile.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace mOfGloalFile {
    export class c {
    }
}
import exports = mOfGloalFile.c;
import require = mOfGloalFile.c;
new exports();
new require();

namespace m1 {
    import exports = mOfGloalFile.c;
    import require = mOfGloalFile.c;
    new exports();
    new require();
}

namespace m2 {
    export import exports = mOfGloalFile.c;
    export import require = mOfGloalFile.c;
    new exports();
    new require();
}