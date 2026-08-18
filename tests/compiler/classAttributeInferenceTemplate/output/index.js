// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classAttributeInferenceTemplate.ts`, Apache-2.0 License
//@compiler-options: target=esnext
class MyClass {
  property;
  property2;
  constructor() {var variable = 'something';
    this.property = `foo`;
    this.property2 = `foo-${variable}`;
    var localProperty = `foo-${variable}`;}
}
class MyClass2 {
  property;
  property2;
  constructor() {var variable = 'something';
    this.property = `foo`;
    this.property2 = `foo-${variable}`;
    var localProperty = `foo-${variable}`;}
}