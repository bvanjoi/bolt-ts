// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/overloadOnConstInheritance3.ts`, Apache-2.0 License

interface Base {
  addEventListener(x: string): any;
}
interface Deriver extends Base {
  // shouldn't need to redeclare the string overload
  addEventListener(x: 'bar'): string;
  addEventListener(x: 'foo'): string;
}
