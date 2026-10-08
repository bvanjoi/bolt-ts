// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/simplifyingConditionalWithInteriorConditionalIsRelated.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
// from https://github.com/microsoft/TypeScript/issues/30706
function ConditionalOrUndefined() {
  return 0;
}
function JustConditional() {
  return ConditionalOrUndefined();// shouldn't error
  
}
// For comparison...
function genericOrUndefined() {
  return 0;
}
function JustGeneric() {
  return genericOrUndefined();// no error
  
}
// Simplified example:
function f() {
  var x = null;
}