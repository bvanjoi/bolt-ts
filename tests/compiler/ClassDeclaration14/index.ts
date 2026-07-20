// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/ClassDeclaration14.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C {
   foo();
   //~^ ERROR: Function implementation is missing or not immediately following the declaration.
   constructor();
   //~^ ERROR: Constructor implementation is missing.
}
