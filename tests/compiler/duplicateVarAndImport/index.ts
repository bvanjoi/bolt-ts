// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/duplicateVarAndImport.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// no error since module is not instantiated

var a;
namespace M { }
import a = M;