// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implicitAnyFunctionReturnNullOrUndefined.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: noImplicitAny
function nullWidenFunction() {
  return null;
}
function undefinedWidenFunction() {
  return undefined;
}
class C {
  nullWidenFuncOfC() {
    return null;
  }
  underfinedWidenFuncOfC() {
    return undefined;
  }
}
function foo1() {
  return null;
}
function bar1() {
  return undefined;
}
function fooBar() {
  return 1;
}
function fooFoo() {
  return 5;
}
nullWidenFunction();
undefinedWidenFunction();