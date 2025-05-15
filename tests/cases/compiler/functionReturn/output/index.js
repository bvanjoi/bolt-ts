// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionReturn.ts`, Apache-2.0 License
function f0() {}
function f1() {
  var n = f0();
}
function f2() {}
function f3() {
  return 
}
function f4() {
  return ""
  return 
}
function f5() {
  return ""
  return undefined
}