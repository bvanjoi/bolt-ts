// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/indexSignatureWithoutTypeAnnotation1.ts`, Apache-2.0 License

class C {
  [a: number];
  //~^ ERROR:  An index signature must have a type annotation.
}