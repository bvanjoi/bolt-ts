export function test(singletons, i) {
  var singleton = singletons[i];
  var [, ...rest] = singleton;
  return rest
}