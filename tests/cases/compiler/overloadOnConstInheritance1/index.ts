// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/overloadOnConstInheritance1.ts`, Apache-2.0 License

interface Base {
  addEventListener(x: string): any;
  addEventListener(x: 'foo'): string;
}
interface Deriver extends Base {
  addEventListener(x: string): any;
  addEventListener(x: 'bar'): string;
}
