// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/errorCause.ts`, Apache-2.0 License

//@[target=ES2021]  compiler-options: target=es2021
//@[target=ES2022]  compiler-options: target=es2022
//@[target=ES2022]  run-fail
//@[target=ESNext]  compiler-options: target=esnext
//@[target=ESNext]  run-fail

declare const a: unknown;

let err = new Error("foo", { cause: new Error("bar") });
//~[target=ES2021]^ ERROR: Expected 0-1 arguments, but got 2.
err.cause;
//~[target=ES2021]^ ERROR: Property 'cause' does not exist on type 'Error'.
let anotherErr = new Error("foo", { cause: a });
//~[target=ES2021]^ ERROR: Expected 0-1 arguments, but got 2.
anotherErr.cause;
//~[target=ES2021]^ ERROR: Property 'cause' does not exist on type 'Error'.

new EvalError("foo", { cause: new Error("bar") });
//~[target=ES2021]^ ERROR: Expected 0-1 arguments, but got 2.
new EvalError("foo", { cause: a });
//~[target=ES2021]^ ERROR: Expected 0-1 arguments, but got 2.
new RangeError("foo", { cause: new Error("bar") });
//~[target=ES2021]^ ERROR: Expected 0-1 arguments, but got 2.
new ReferenceError("foo", { cause: new Error("bar") });
//~[target=ES2021]^ ERROR: Expected 0-1 arguments, but got 2.
new SyntaxError("foo", { cause: new Error("bar") });
//~[target=ES2021]^ ERROR: Expected 0-1 arguments, but got 2.
new TypeError("foo", { cause: new Error("bar") });
//~[target=ES2021]^ ERROR: Expected 0-1 arguments, but got 2.
new URIError("foo", { cause: new Error("bar") });
//~[target=ES2021]^ ERROR: Expected 0-1 arguments, but got 2.
new AggregateError([], "foo", { cause: new Error("bar") });
//~[target=ES2021]^ ERROR: Expected 1-2 arguments, but got 3.
