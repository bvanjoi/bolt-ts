interface Action<TName extends string, TPayload> {
  name: TName;
  payload: TPayload;
}
type A = {
  kind: "a";
};
type B = {
  kind: "b";
};
declare var a: A;
declare var b: B;
declare function fab(arg: A | B): void;
declare function foo<T>(x: {
  kind: T;
}, f: (arg: {
  kind: T;
}) => void): void;


interface Action<TName extends string, TPayload> {
  name: TName;
  payload: TPayload;
}
declare var actionA: Action<"ACTION_A", string>;
declare var actionB: Action<"ACTION_B", boolean>;
declare function call<TName extends string, TPayload>(action: Action<TName, TPayload>, fn: (action: Action<TName, TPayload>) => any): void;
declare var printFn: (action: Action<"ACTION_A", string> | Action<"ACTION_B", boolean>) => void;


