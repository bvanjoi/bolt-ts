var FOO_SYMBOL = Symbol('Foo');

export function foo(p) {
  p[FOO_SYMBOL] = 3;
}