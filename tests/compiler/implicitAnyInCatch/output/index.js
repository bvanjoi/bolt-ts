// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implicitAnyInCatch.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: noImplicitAny
//@compiler-options: useUnknownInCatchVariables=false
try {} catch (error) {
  if (error.number === -2147024809) {}
  
}
for ( var key in this) {}
class C {
  temp() {
    for ( var x in this) {}
  }
}