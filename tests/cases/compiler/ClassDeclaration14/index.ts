// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/ClassDeclaration14.ts`, Apache-2.0 License

class C {
   foo();
   //~^ ERROR: Function implementation is missing or not immediately following the declaration.
   constructor();
   //~^ ERROR: Constructor implementation is missing.
}