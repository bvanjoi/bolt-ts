// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/importNonExportedMember3.ts`, Apache-2.0 License

//@compiler-options: module=commonjs
//@compiler-options: target=es2015

import X from 'file';
//~^ ERROR: Cannot find module 'file' or its corresponding type declarations.
import Z from 'other_file';
//~^ ERROR: Cannot find module 'other_file' or its corresponding type declarations.

class Y extends Z {
  constructor() {
    super(X);
  }
}

import X2 from 'file2';
//~^ ERROR: Cannot find module 'file2' or its corresponding type declarations.
import X3 from 'file3';
//~^ ERROR: Cannot find module 'file3' or its corresponding type declarations.
class Q extends Z {
  constructor() {
    super(X2, X3);
  }
}