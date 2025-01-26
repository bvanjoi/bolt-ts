// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/anyPlusAny1.ts`, Apache-2.0 License

//@ run-fail

var x: any;
x.name = "hello";
var z = x + x;