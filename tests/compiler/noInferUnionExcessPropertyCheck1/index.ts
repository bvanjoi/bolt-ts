// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noInferUnionExcessPropertyCheck1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare function test1<T extends { x: string }>(
  a: T,
  b: NoInfer<T> | (() => NoInfer<T>),
): void;

test1({ x: "foo" }, { x: "bar" }); // no error
test1({ x: "foo" }, { x: "bar", y: 42 }); // epc error
//~^ ERROR: Object literal may only specify known properties, and 'y' does not exist in type 'substitution | (() => substitution)'.

declare function test2<T extends { x: string }>(
  a: T,
  b: NoInfer<T> | NoInfer<() => T>,
): void;

test2({ x: "foo" }, { x: "bar" }); // no error
test2({ x: "foo" }, { x: "bar", y: 42 }); // epc error
//~^ ERROR: Object literal may only specify known properties, and 'y' does not exist in type 'substitution | substitution'.

declare function test3<T extends { x: string }>(
  a: T,
  b: NoInfer<T | (() => T)>,
): void;

test3({ x: "foo" }, { x: "bar" }); // no error
test3({ x: "foo" }, { x: "bar", y: 42 }); // epc error
//~^ ERROR: Object literal may only specify known properties, and 'y' does not exist in type '{ x: string; } | (() => { x: string; })'.
