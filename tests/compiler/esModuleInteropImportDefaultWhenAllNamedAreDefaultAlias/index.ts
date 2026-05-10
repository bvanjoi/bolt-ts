// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/esModuleInteropImportDefaultWhenAllNamedAreDefaultAlias.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: esModuleInterop

import {default as a, default as b} from "m"; //~ERROR: Cannot find module 'm' or its corresponding type declarations.
void a;
void b;