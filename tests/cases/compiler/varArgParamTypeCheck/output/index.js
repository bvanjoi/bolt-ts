// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/varArgParamTypeCheck.ts`, Apache-2.0 License
function sequence(...sequences) {}
function callback(clb) {}
sequence(function bar() {}, function foo() {
  callback(() => {
    this();
  });
}, function baz() {
  callback(() => {
    this();
  });
});