// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nonNullParameterExtendingStringAssignableToString.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function fn(one, two) {
  var three = Boolean() ? one : two;
  foo(one);
  foo(two);
  foo(three);
}