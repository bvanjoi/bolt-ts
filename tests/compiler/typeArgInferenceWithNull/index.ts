// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeArgInferenceWithNull.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// All legal

function fn4<T extends string>(n: T) { }
fn4(null);
//~^ ERROR: Argument of type 'null' is not assignable to parameter of type 'string'.

function fn5<T extends { x: string }>(n: T) { }
fn5({ x: null });
//~^ ERROR: Type 'null' is not assignable to type 'string'.

function fn6<T extends { x: string }>(n: T, fun: (x: T) => void, n2: T) { }
fn6({ x: null }, y => { }, { x: "" }); // y has type { x: any }, but ideally would have type { x: string }
//~^ ERROR: Type 'null' is not assignable to type 'string'.
