// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classExpressionWithStaticProperties2.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=es5]     compiler-options: target=es5
//@[target=es2015]  compiler-options: target=es2015

var v = class C {
    static a = 1;
    static b
    static c = {
        x: "hi"
    }
    static d = C.c.x + " world";
 };