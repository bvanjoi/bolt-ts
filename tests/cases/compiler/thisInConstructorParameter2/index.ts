// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisInConstructorParameter2.ts`, Apache-2.0 License

class P {
  x = this;
  static y = this;

  constructor(public z = this, zz = this, zzz = (p = this) => this) {
      zzz = (p = this) => this;
      zz.x;
  }

  foo(zz = this) { zz.x; this.x; }
  static bar(zz = this) { zz.y; }
}