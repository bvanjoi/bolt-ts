// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/intersectionTypeWithLeadingOperator.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type A = & string;
type B =
  & { foo: string }
  & { bar: number };

type C = [& { foo: 1 } & { bar: 2 }, & { foo: 3 } & { bar: 4 }];
