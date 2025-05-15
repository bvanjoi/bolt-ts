// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/overloadOnConstNoNonSpecializedSignature.ts`, Apache-2.0 License
class C {
  
  // error, no non-specialized signature in overload list
  x1(a) {}
}