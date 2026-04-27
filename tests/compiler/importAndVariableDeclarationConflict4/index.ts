// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/importAndVariableDeclarationConflict4.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace m {
  export var m = '';
}

var x = '';
import x = m.m;
//~^ ERROR: Import declaration conflicts with local declaration of 'x'.
