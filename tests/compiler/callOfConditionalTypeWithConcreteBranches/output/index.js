function fn(arg) {
  arg(10);
}
fn((m) => (m.toFixed()));
fn((m) => (m.toFixed()));
function fn2(arg) {
  function useT(_arg) {}
  arg((arg) => (useT(arg)));
}
fn2((m) => (m(42)));
fn2((m) => (m(42)));