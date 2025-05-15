// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/accessors_spec_section-4.5_inference.ts`, Apache-2.0 License
class A {}
class B extends A {}
class LanguageSpec_section_4_5_inference {
  set InferredGetterFromSetterAnnotation(a) {}
  get InferredGetterFromSetterAnnotation() {
    return new B()
  }
  get InferredGetterFromSetterAnnotation_GetterFirst() {
    return new B()
  }
  set InferredGetterFromSetterAnnotation_GetterFirst(a) {}
  get InferredFromGetter() {
    return new B()
  }
  set InferredFromGetter(a) {}
  set InferredFromGetter_SetterFirst(a) {}
  get InferredFromGetter_SetterFirst() {
    return new B()
  }
  set InferredSetterFromGetterAnnotation(a) {}
  get InferredSetterFromGetterAnnotation() {
    return new B()
  }
  get InferredSetterFromGetterAnnotation_GetterFirst() {
    return new B()
  }
  set InferredSetterFromGetterAnnotation_GetterFirst(a) {}
}