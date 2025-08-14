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