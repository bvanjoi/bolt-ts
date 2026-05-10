// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contravariantOnlyInferenceFromAnnotatedFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type Funcs<A, B extends Record<string, unknown>> = {
  [K in keyof B]: {
    fn: (a: A, b: B) => void;
    thing: B[K];
  };
}

declare function foo<A, B extends Record<string, unknown>>(fns: Funcs<A, B>): [A, B]

const result = foo({
  bar: {
    fn: (a: string) => {},
    thing: 'asd',
  },
});
