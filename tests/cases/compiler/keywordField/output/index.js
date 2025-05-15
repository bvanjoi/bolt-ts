// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/keywordField.ts`, Apache-2.0 License
var obj = {};
obj.if = 1;
var a = {if: "test"};
var n = a.if;
var q = a["if"];