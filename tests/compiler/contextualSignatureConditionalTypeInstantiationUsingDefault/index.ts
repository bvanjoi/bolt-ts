// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualSignatureConditionalTypeInstantiationUsingDefault.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

export interface TypegenDisabled {
  "@@xstate/typegen": false;
}
export interface TypegenEnabled {
  "@@xstate/typegen": true;
}

type ActionFunction<TEvent extends { type: string }> = (event: TEvent) => void;

declare function createMachine<
  TTypesMeta extends TypegenEnabled | TypegenDisabled = TypegenDisabled
>(
  config: {
    types?: TTypesMeta;
  },
  implementations: TTypesMeta extends TypegenEnabled
    ? ActionFunction<{ type: "test" }>
    : ActionFunction<{ type: string }>
): void;

createMachine({}, (ev) => {
  ev.type; // should be `string`
});
