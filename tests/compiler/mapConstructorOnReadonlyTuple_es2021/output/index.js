//@compiler-options: target=es2021
var pairs = [[{}, 1], [{}, 2]];
new Map(pairs);
new WeakMap(pairs);
new Map([['', {
  key: undefined  
}]]);
{}