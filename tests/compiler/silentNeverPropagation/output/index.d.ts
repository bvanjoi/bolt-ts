type ModuleWithState<TState> = {
  state: TState;
};
type State = {
  a: number;
};
type MoreState = {
  z: string;
};
declare function createModule<TState, TActions>(state: TState, actions: TActions): ModuleWithState<TState> & TActions;
declare function convert<TState, TActions>(m: ModuleWithState<TState> & TActions): ModuleWithState<TState & MoreState> & TActions;
declare var breaks: ModuleWithState<{ a: number; } & MoreState> & ModuleWithState<{ a: number; }> & { foo: () => true; };



