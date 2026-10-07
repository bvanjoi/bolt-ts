// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/callOfConditionalTypeWithConcreteBranches.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function fn(arg) {
  // Expected: OK
  // Actual: Cannot convert 10 to number & T
  arg(10);
}
// Legal invocations are not problematic
fn((m) => (m.toFixed()));
fn((m) => (m.toFixed()));// Ensure the following real-world example that relies on substitution still works
// The above allows "parameters" to index `T` since all later
// instances are actually implicitly `"parameters" & keyof T`
// Original example, but with inverted variance

function fn2(arg) {
  function useT(_arg) {}
  // Expected: OK
  arg((arg) => (useT(arg)));
}
// Legal invocations are not problematic
fn2((m) => (m(42)));
fn2((m) => (m(42)));// webidl-conversions example where substituion must occur, despite contravariance of the position
// due to the invariant usage in `Parameters`
// vscode - another `Parameters` example
