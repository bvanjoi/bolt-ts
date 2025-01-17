// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/keywordField.ts`, Apache-2.0 License

var obj:any = {};

obj.if = 1;

var a = { if: "test" }

var n = a.if

var q = a["if"];
