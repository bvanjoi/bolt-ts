// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reverseMappedTypeInferenceSameSource1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type Action<T extends string = string> = {
  type: T;
};
interface UnknownAction extends Action {
  [extraProps: string]: unknown;
}
type Reducer<S = any, A extends Action = UnknownAction> = (
  state: S | undefined,
  action: A,
) => S;

type ReducersMapObject<S = any, A extends Action = UnknownAction> = {
  [K in keyof S]: Reducer<S[K], A>;
};

interface ConfigureStoreOptions<S = any, A extends Action = UnknownAction> {
  reducer: Reducer<S, A> | ReducersMapObject<S, A>;
}

declare function configureStore<S = any, A extends Action = UnknownAction>(
  options: ConfigureStoreOptions<S, A>,
): void;

{
  const reducer: Reducer<number> = () => 0;
  const store1 = configureStore({ reducer });
}

const counterReducer1: Reducer<number> = () => 0;

const store2 = configureStore({
  reducer: {
    counter1: counterReducer1,
  },
});

export {}
