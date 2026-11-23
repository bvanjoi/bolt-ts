type inferPipe<t, pipe> = pipe extends (In: t) => unknown ? (In: t) => ReturnType<pipe> : never;
interface Type<t> {
  pipe<fn extends (In: t) => unknown>(fn: fn): Type<inferPipe<t, fn>>;
}
var t: Type<string>;
export var out: Type<(In: string) => number>;
type inferPipe2<t, pipe> = pipe extends (In: t) => unknown ? (In: t) => ReturnType<pipe> extends infer n extends number ? n : ReturnType<pipe> extends infer s extends string ? s : ReturnType<pipe> extends infer b extends boolean ? b : never : never;
interface Type2<t> {
  pipe<fn extends (In: t) => unknown>(fn: fn): Type<inferPipe2<t, fn>>;
}
var t2: Type2<string>;
export var out2: Type<(In: string) => number>;
