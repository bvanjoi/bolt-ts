
// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeLiteralCallback.ts`, Apache-2.0 License
//@ run-fail
var foo;
foo.reject("");

var test;
test.fail((arg) => foo.reject(arg));
test.fail2((arg) => foo.reject(arg));