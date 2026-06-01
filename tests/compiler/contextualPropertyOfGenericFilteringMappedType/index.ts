// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualPropertyOfGenericFilteringMappedType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare function f1<T extends object>(
  data: T,
  handlers: { [P in keyof T as P]: (value: T[P], prop: P) => void },
): void;

f1(
  {
    foo: 0,
    bar: "",
  },
  {
    foo: (value, key) => {},
    bar: (value, key) => {},
  },
);

declare function f2<T extends object>(
  data: T,
  handlers: { [P in keyof T as T[P] extends string ? P : never]: (value: T[P], prop: P) => void },
): void;

f2(
  {
    foo: 0,
    bar: "",
  },
  {
    bar: (value, key) => {},
  },
);

f2(
  {
    foo: 0,
    bar: "",
  },
  {
    foo: (value, key) => {
      // implicit `any`s
      //~^^ ERROR: Parameter 'value' implicitly has an 'any' type.
      //~| ERROR: Parameter 'key' implicitly has an 'any' type.
      //~| ERROR: Object literal may only specify known properties, and 'foo' does not exist in type 'mapped type'.
    },
  },
);
