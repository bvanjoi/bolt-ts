// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisInArrowFunctionInStaticInitializer1.ts`, Apache-2.0 License

function log(a) { }

class Vector {
 static foo = () => {
  // 'this' should be allowed in a static initializer.
  log(this);
 }
}