// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/paramsOnlyHaveLiteralTypesWhenAppropriatelyContextualized.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type Lower<T> = { [K in keyof T]: T[K] };

export function appendToOptionalArray<
  K extends string | number | symbol,
  T
>(
  object: { [x in K]?: Lower<T>[] },
  key: K,
  value: T
) {
  const array = object[key];
  if (array) {
    array.push(value);
  } else {
    object[key] = [value];
  }
}

// e.g.
const foo: {x?: number[]; y?: string[]; } = {};
appendToOptionalArray(foo, 'x', 123);   // ok
appendToOptionalArray(foo, 'y', 'bar'); // ok
appendToOptionalArray(foo, 'y', 12);    // should fail
//~^ ERROR: Argument of type '{ x: undefined | number[]; y: undefined | string[]; }' is not assignable to parameter of type '{ }'.
appendToOptionalArray(foo, 'x', "no");  // should fail
//~^ ERROR: Argument of type '{ x: undefined | number[]; y: undefined | string[]; }' is not assignable to parameter of type '{ }'.
