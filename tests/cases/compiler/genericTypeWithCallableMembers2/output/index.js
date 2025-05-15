// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericTypeWithCallableMembers2.ts`, Apache-2.0 License
function foo1(f) {
  return f()
}
// should return 'string', once returned 'any'
function foo2(f) {
  return new f()
}
// should be legal, once was an error
var a = foo1(() => "hello");
