declare namespace a {
  export function foo(x: number): number;
}
declare namespace c {
  import b = a.foo;
  var bVal: number;
  var bVal2: (x: number) => number;
}
