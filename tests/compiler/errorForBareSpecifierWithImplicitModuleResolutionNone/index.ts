// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/errorForBareSpecifierWithImplicitModuleResolutionNone.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=es2015

import { thing } from "non-existent-module";  //~ERROR: Cannot find module 'non-existent-module' or its corresponding type declarations.
thing()
