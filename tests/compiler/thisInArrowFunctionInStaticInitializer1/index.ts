// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/thisInArrowFunctionInStaticInitializer1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function log(a) { }

class Vector {
 static foo = () => {
  // 'this' should be allowed in a static initializer.
  log(this);
 }
}
