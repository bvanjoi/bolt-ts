// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/duplicateVarAndImport2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var a;
namespace M { export var x = 1; }
import a = M;
//~^ ERROR: Import declaration conflicts with local declaration of 'a'.