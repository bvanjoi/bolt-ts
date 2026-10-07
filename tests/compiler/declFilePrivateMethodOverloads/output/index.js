// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFilePrivateMethodOverloads.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: declaration
class c1 {
  _forEachBindingContext(context, fn) {// Function here
  }
  overloadWithArityDifference(context) {// Function here
  }
}