// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/ClassDeclaration10.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C {
  constructor(); //~ ERROR: Constructor implementation is missing.
  foo();         //~ ERROR: Function implementation is missing or not immediately following the declaration.
}
