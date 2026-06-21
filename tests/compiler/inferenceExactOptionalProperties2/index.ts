// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferenceExactOptionalProperties2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: exactOptionalPropertyTypes
//@compiler-options: noEmit

declare function setup2<TTTT extends Record<string, {}>>(n: { [K in keyof TTTT]: TTTT[K] }): {
  [J in keyof TTTT]: J;
};
const b0: { cccc: 42 } = { cccc: 42 } as const;
const a0: '42' = setup2(b0)['cccc'];
//~^ ERROR: Type '"cccc"' is not assignable to type '"42"'.

type Values<T> = T[keyof T];

type EventObject = {
  type: string;
};

interface ActorLogic<TEvent extends EventObject> {
  transition: (ev: TEvent) => unknown;
}

type UnknownActorLogic = ActorLogic<never>;

interface ProvidedActor {
  src: string;
  logic: UnknownActorLogic;
}

interface ActionFunction<TActor extends ProvidedActor> {
  (): void;
  _out_TActor?: TActor;
}

interface AssignAction<TActor extends ProvidedActor> {
  (): void;
  _out_TActor?: TActor;
}

interface MachineConfig<TActor extends ProvidedActor> {
  entry?: ActionFunction<TActor>;
}

declare function assign<TActor extends ProvidedActor>(
  _: (spawn: (actor: TActor["src"]) => void) => {},
): AssignAction<TActor>;

type ToProvidedActor<TActors extends Record<string, UnknownActorLogic>> =
  Values<{
    [K in keyof TActors & string]: {
      src: K;
      logic: TActors[K];
    };
  }>;

declare function setup<
  TActors extends Record<string, UnknownActorLogic> = {},
>(implementations?: {
  actors?: { [K in keyof TActors]: TActors[K] };
}): {
  createMachine: <
    const TConfig extends MachineConfig<ToProvidedActor<TActors>>,
  >(
    config: TConfig,
  ) => void;
};

declare const counterLogic: ActorLogic<{ type: "INCREMENT" }>;

// example usage
setup({
  actors: { counter: counterLogic },
}).createMachine({
  entry: assign((spawn) => {
    spawn("counter"); // ok
    spawn("alarm"); // error
    //~^ ERROR: Argument of type 'string' is not assignable to parameter of type '"counter"'.
    return {};
  }),
});

// no provided actors, `assign` should still work
setup().createMachine({
  entry: assign(() => ({})),
});
