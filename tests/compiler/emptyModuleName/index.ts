// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/emptyModuleName.ts`, Apache-2.0 License

//@compiler-options: module=commonjs

import * as A from "";
//~^ ERROR: Cannot find module '' or its corresponding type declarations.
class B extends A {
}
 