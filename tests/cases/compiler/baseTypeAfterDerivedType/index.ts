// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/baseTypeAfterDerivedType.ts`, Apache-2.0 License

interface Derived extends Base {
  method(...args: any[]): void;
}

interface Base {
  method(...args: any[]): void;
}

class Derived2 implements Base2 {
  method(...args: any[]): void { }
}

interface Base2 {
  method(...args: any[]): void;
}
