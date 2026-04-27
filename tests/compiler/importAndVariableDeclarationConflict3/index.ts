// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/importAndVariableDeclarationConflict3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace m {
  export var m = '';
}

import x = m.m;
import x = m.m;
//~^ ERROR: Duplicate identifier 'x'.
