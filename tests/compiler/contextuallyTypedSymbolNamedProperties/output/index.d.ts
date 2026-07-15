declare const A: unique symbol;

declare const B: unique symbol;

type Action = {
  type: typeof A;
  data: string;
} | {
  type: typeof B;
  data: number;
};
declare const ab: Action;

declare function f<T extends {
  type: string | symbol;
}>(action: T, blah: {
[K in T["type"]]: (p: K) => void
}): any;

declare const x: {
  [sym:symbol]: (p: string) => void;
};

