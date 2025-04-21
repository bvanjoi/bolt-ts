// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/ClassDeclaration10.ts`, Apache-2.0 License
class C {
  constructor(); //~ ERROR: Constructor implementation is missing.
  foo();         //~ ERROR: Function implementation is missing or not immediately following the declaration.
}