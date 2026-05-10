// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferenceAndSelfReferentialConstraint.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type Test<K extends keyof any> = {
  [P in K | "foo"]: P extends "foo" ? true : () => any
}

function test<T extends Test<keyof T>>(arg: T) {
  return arg;
}

const res1 = test({
  foo: true,
  bar() {
  }
});

const res2 = test({
  foo: true,
  bar: function () {
  }
});

const res3 = test({
  foo: true,
  bar: () => {
  }
});
