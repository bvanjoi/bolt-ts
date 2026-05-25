// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/compoundVarDecl1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace Foo { var a = 1, b = 1; a = b + 2; }

var foo = 4, bar = 5;