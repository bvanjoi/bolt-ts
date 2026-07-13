// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/importDeclarationInModuleDeclaration1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace m2 {
    import m3 = require("use_glo_M1_public");
    //~^ ERROR: Import declarations in a namespace cannot reference a module.
}
