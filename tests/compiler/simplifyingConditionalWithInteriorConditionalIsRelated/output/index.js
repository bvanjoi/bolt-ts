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