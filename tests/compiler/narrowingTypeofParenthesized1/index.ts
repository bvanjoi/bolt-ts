// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/narrowingTypeofParenthesized1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare const foo: string;

if ((typeof foo) === "string") {
  foo;
  let a: number = foo;
  //~^ ERROR: Type 'string' is not assignable to type 'number'.
} else {
  foo;
  let a: number = foo;
}

if (typeof foo === ("string")) {
  foo;
  let a: number = foo;
  //~^ ERROR: Type 'string' is not assignable to type 'number'.
} else {
  foo;
  let a: number = foo;
}
