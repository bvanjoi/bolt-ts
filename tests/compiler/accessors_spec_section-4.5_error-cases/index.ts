// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/accessors_spec_section-4.5_error-cases.ts`, Apache-2.0 License

class LanguageSpec_section_4_5_error_cases {
  public set AnnotatedSetter_SetterFirst(a: number) { }
  public get AnnotatedSetter_SetterFirst() { return ""; }
  //~^ ERROR: Type 'string' is not assignable to type 'number'.

  public get AnnotatedSetter_SetterLast() { return ""; }
  //~^ ERROR: Type 'string' is not assignable to type 'number'.
  public set AnnotatedSetter_SetterLast(a: number) { }

  public get AnnotatedGetter_GetterFirst(): string { return ""; }
  public set AnnotatedGetter_GetterFirst(aStr) { aStr = 0; }
  //~^ ERROR: Type 'number' is not assignable to type 'string'.

  public set AnnotatedGetter_GetterLast(aStr) { aStr = 0; }
  //~^ ERROR: Type 'number' is not assignable to type 'string'.
  public get AnnotatedGetter_GetterLast(): string { return ""; }
}