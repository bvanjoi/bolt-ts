interface I {
  foo(...x: any[]): typeof x;
  foo2(a: number, ...x: any[]): typeof x;
  foo3(b: string, ...x: string[]): typeof x;
}
