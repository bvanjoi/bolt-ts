function tag(parts, ...values) {
  return parts[0]
}
function foo() {
  tag`foo`;
  tag`foo2`;
}
function bar() {
  tag`bar`;
  tag`bar2`;
}
foo();
bar();
{
  function f(a) {}
  f``;
}