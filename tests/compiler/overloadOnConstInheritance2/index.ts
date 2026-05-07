// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/ambiguousOverload2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Base {
    addEventListener(x: string): any;
    addEventListener(x: 'foo'): string;
}
interface Deriver extends Base {
  //~^ ERROR: Interface 'Deriver' incorrectly extends interface 'Base'.
    addEventListener(x: 'bar'): string; // shouldn't need to redeclare the string overload
}