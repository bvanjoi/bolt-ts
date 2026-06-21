declare class A {
  foo(): void;
}
declare class B {
  bar(): void;
}
interface I {
  baz(): any;
}
interface J {
  bat(): any;
}
declare class D  implement I, J {
  baz(): void;
  bat(): void;
  foo(): void;
  bar(): void;
}
interface I extends AB {}
