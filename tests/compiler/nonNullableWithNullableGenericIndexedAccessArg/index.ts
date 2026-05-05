// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nonNullableWithNullableGenericIndexedAccessArg.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

interface StateSchema {
  states?: {
    [key: string]: StateSchema;
  };
}

declare class StateNode<TStateSchema extends StateSchema> {
  schema: TStateSchema;
}

type StateNodesConfig<TStateSchema extends StateSchema> = {
  [K in keyof TStateSchema["states"]]: StateNode<NonNullable<TStateSchema["states"]>[K]>;
};

// repro from #50539#issuecomment-1234067835

type Ordering<TOrderBy extends string> = {
    orderBy: TOrderBy
}

type Query<TOrderBy extends string> = {
    order?: Ordering<TOrderBy>
}

type QueryHandler<
    TQuery extends Query<TOrderBy>,
    TOrderBy extends string = NonNullable<TQuery["order"]>["orderBy"]
> = {}
