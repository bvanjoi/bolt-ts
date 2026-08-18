declare var A: unique symbol;
declare var B: unique symbol;
type Action = {
  type: typeof A;
  data: string;
} | {
  type: typeof B;
  data: number;
};
declare var ab: Action;
declare function f<T extends {
  type: string | symbol;
}>(action: T, blah: {
[K in T["type"]]: (p: K) => void
}): any;

declare var x: {
  [sym:symbol]: (p: string) => void;
};
