// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyUnionNormalizedObjectLiteral1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks=false
//@compiler-options: noImplicitAny
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/58150

function doSthWithParams(params: unknown) {
  if (typeof params !== "object") {
    return {};
  }

  return {
    c: "foo",
    p: "bar",
    s: "baz",
  };
}

const bar = {
  p: null,
  //~^ ERROR: Object literal's property 'p' implicitly has an 'any' type.
  s: null,
  //~^ ERROR: Object literal's property 's' implicitly has an 'any' type.
  ...doSthWithParams({
    p: "hello",
    s: "world",
  }),
};
