// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileTypeAnnotationBuiltInType.ts`, Apache-2.0 License
//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: declaration
// string
function foo() {
  return '';
}
function foo2() {
  return '';
}
// number
function foo3() {
  return 10;
}
function foo4() {
  return 10;
}
// boolean
function foo5() {
  return true;
}
function foo6() {
  return false;
}
// void
function foo7() {
  return ;
}
function foo8() {
  return ;
}
// any
function foo9() {
  return undefined;
}
function foo10() {
  return undefined;
}