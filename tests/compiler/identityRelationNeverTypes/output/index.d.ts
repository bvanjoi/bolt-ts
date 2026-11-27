type Equals<A, B> = (<T>() => T extends B ? 1 : 0) extends (<T>() => T extends A ? 1 : 0) ? true : false;
declare class State <TContext>{
  _context: TContext;
  _value: string;
  matches<TSV extends string>(stateValue: TSV): this is State<TContext> & {
    value: TSV;
  };
}
declare function f1(state: State<{
  foo: number;
}>): void;
