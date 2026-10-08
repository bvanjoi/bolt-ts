// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classVarianceResolveCircularity2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
export {  }
class Bar {
  num;
  Value = callme(new Foo(this)).bar.num;
// Field: number = callme(new Foo(this)).bar.num;
}
class Foo {
  bar;
  constructor(bar) {this.bar = bar;}
}