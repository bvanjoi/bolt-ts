// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/forInStatement5.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var a: string;
var expr: any;
for (a in expr) {
}
