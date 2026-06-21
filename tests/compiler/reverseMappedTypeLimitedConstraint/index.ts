// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reverseMappedTypeLimitedConstraint.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type XNumber_ = { x: number }

declare function foo_<T extends XNumber_>(props: {[K in keyof T & keyof XNumber_]: T[K]}): T;

foo_({x: 1, y: 'foo'});
//~^ ERROR: Object literal may only specify known properties, and 'y' does not exist in type '{ x: T["x"]; }'.
// -----------------------------------------------------------------------------------------

const checkType_ = <T>() => <U extends T>(value: { [K in keyof U & keyof T]: U[K] }) => value;

const checked_ = checkType_<{x: number, y: string}>()({
  x: 1 as number,
  y: "y",
  z: "z",
//~^ ERROR: Object literal may only specify known properties, and 'z' does not exist in type '{ }'.
});
