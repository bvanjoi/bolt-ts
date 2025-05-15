// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typedGenericPrototypeMember.ts`, Apache-2.0 License
class List {
  add(item) {}
}
List.prototype.add("abc");