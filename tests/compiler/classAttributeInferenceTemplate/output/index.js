// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classAttributeInferenceTemplate.ts`, Apache-2.0 License
//@compiler-options: target=esnext
//@compiler-options: strict
class MyClass {
  property;
  property2;
  constructor() {var variable = 'something';
    this.property = `foo`// Correctly inferred as `string`
    ;
    this.property2 = `foo-${variable}`// Causes an error
    ;
    var localProperty = `foo-${variable}`// Correctly inferred as `string`
    ;}
}
class MyClass2 {
  property;
  property2;
  constructor() {var variable = 'something';
    this.property = `foo`// Correctly inferred as `string`
    ;
    this.property2 = `foo-${variable}`// Causes an error
    ;
    var localProperty = `foo-${variable}`// Correctly inferred as `string`
    ;}
}