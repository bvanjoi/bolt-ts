// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/genericConstructSignatureInInterface.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

interface C {
    new <T>(x: T);
}

var v: C;
var r = new v<number>(1);