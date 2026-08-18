// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/simplifyingConditionalWithInteriorConditionalIsRelated.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
function ConditionalOrUndefined() {
  return 0;
}
function JustConditional() {
  return ConditionalOrUndefined();
}
function genericOrUndefined() {
  return 0;
}
function JustGeneric() {
  return genericOrUndefined();
}
function f() {
  var x = null;
}