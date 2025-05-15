// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/arrowFunctionWithObjectLiteralBody5.ts`, Apache-2.0 License
var a = () => ({name: "foo",
message: "bar"});
var b = () => ({name: "foo",
message: "bar"});
var c = () => ({name: "foo",
message: "bar"});
var d = () => ((({name: "foo",
message: "bar"})));
({name: "foo",
message: "bar"});
var f = {name: "foo",
message: "bar"};