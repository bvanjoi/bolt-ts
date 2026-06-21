// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/keyRemappingKeyofResult2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type Values<T> = T[keyof T];

type ProvidedActor = {
  src: string;
  logic: unknown;
};

interface StateMachineConfig<TActors extends ProvidedActor> {
  invoke: {
    src: TActors["src"];
  };
}

declare function setup<TActors extends Record<string, unknown>>(_: {
  actors: {
    [K in keyof TActors]: TActors[K];
  };
}): {
  createMachine: (
    config: StateMachineConfig<
      Values<{
        [K in keyof TActors as K & string]: {
          src: K;
          logic: TActors[K];
        };
      }>
    >,
  ) => void;
};
