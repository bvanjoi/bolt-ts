// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/classExpressionWithStaticProperties1.ts`, Apache-2.0 License

var v = class C {
    static a = 1;
    static b = 2;
    static c = C.a + C.b;
};