declare class A {
  foo();
}
declare class B {
  bar();
}
interface I {
  baz(): any;
}
interface J {
  bat(): any;
}
declare class D {
  baz();
  bat();
  foo();
  bar();
}
interface I extends AB {}
