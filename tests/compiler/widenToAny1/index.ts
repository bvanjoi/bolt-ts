// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/widenToAny1.ts`, Apache-2.0 License

function foo1<T>(f1: { x: T; y: T }): T {
  return undefined;
}
var z1: number = foo1({ x: undefined, y: "def" });  // Best common type is any
//~^ ERROR: Type 'string' is not assignable to type 'number'.
