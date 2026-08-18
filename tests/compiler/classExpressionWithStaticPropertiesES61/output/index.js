// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classExpressionWithStaticPropertiesES61.ts`, Apache-2.0 License
var v = class C {
  static a = 1;
  static b = 2;
  static c = C.a + 3;
};