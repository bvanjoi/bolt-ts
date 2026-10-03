// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/importAnImport.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace c.a.b {
    import ma = a;
}

namespace m0 {
    import m8 = c.a.b.ma;
    //~^ ERROR: Namespace 'b' has no exported member 'ma'.
}
