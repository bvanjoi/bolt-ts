// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/ClassDeclaration9.ts`, Apache-2.0 License

class C {
  foo(); //~ ERROR: Function implementation is missing or not immediately following the declaration.
}