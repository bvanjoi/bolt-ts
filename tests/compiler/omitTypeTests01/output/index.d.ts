interface Foo {
  a: string;
  b: number;
  c: boolean;
}
type Bar = Omit<Foo, "c">;
type Baz = Omit<Foo, "b" | "c">;
export function getBarA(bar: Bar): string;
export function getBazA(baz: Baz): string;
