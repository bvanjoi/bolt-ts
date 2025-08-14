//@compiler-options: target=es2015

const pairs = [[{}, 1], [{}, 2]] as const;
new Map(pairs);
new WeakMap(pairs);
new Map([['', {key: undefined}]])
