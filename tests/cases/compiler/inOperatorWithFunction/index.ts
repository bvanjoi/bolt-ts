// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/inOperatorWithFunction.ts`, Apache-2.0 License

var fn = function (val: boolean) { return val; }
fn("a" in { "a": true });
