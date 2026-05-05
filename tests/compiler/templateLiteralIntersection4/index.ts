// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/templateLiteralIntersection4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

type StateHook<S> = () => [S, unknown];

type StoreUtils<Store extends { [K: string]: any }> = Omit<{
    [K in keyof Store as `use${Capitalize<string & K>}`]: StateHook<Store[K]>
}, 'useStore'> & {
  Provider: unknown,
  useStore: StateHook<Store>
};

declare function createStore<Store extends { [K: string]: any }>(store: Store): StoreUtils<Store>;

const { Provider, useUsername, useAge, useStore } = createStore({
  username: "Aral",
  age: 31
});
