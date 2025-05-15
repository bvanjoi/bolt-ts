// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/targetTypeTest2.ts`, Apache-2.0 License
var a = [1, 2, "3"];
function func1(stuff) {
  return stuff
}
function func2(stuff1, stuff2, stuff3) {
  return func1([stuff1, stuff2, stuff3])
}