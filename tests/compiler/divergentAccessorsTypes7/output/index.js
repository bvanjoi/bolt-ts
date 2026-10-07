// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/divergentAccessorsTypes7.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class Test {
  constructor() {}
  set value(value) {}
  get value() {
    return null;
  }
// -- Replacing the getter such that the getter/setter types match, removes the error:
// get value(): string | ((item: S) => string) {
//     return null!;
// }
// -- Or, replacing the setter such that a concrete type is used, removes the error:
// set value(value: string | ((item: { property: string }) => string)) {}
}
var a = new Test();
a.value = (item) => (item.property);
a['value'] = (item) => (item.property);