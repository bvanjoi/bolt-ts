// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadOnConstNoNonSpecializedSignature.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
class C {
  // error, no non-specialized signature in overload list
  x1(a) {}
}