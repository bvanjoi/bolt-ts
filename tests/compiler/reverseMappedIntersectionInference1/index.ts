// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reverseMappedIntersectionInference1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type Results<T> = {
  [K in keyof T]: {
    data: T[K];
    onSuccess: (data: T[K]) => void;
  };
};

type Errors<E> = {
  [K in keyof E]: {
    error: E[K];
    onError: (data: E[K]) => void;
  };
};

declare function withKeyedObj<T, E>(
  arg: Results<T> & Errors<E>
): [T, E];

const res = withKeyedObj({
  a: {
    data: "foo",
    onSuccess: (dataArg) => {
      dataArg;
    },
    error: 404,
    onError: (errorArg) => {
      errorArg;
    },
  },
  b: {
    data: true,
    onSuccess: (dataArg) => {
      dataArg;
    },
    error: 500,
    onError: (errorArg) => {
      errorArg;
    },
  },
});
