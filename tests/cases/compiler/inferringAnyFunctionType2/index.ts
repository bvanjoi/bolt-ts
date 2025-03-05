function f<T extends [(p1: number) => number]>(p: T): T {
  return p;
}

var v = f([x => x]);
f([x => {
  let j: string = x;
  //~^ ERROR: Type 'number' is not assignable to type 'string'.
  return x
}])