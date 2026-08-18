// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/inferringAnyFunctionType2.ts`, Apache-2.0 License

function f<T extends [(p1: number) => number]>(p: T): T {
  return p;
}

var v = f([x => x]);
f([x => {
  let j: string = x;
  //~^ ERROR: Type 'number' is not assignable to type 'string'.
  return x
}])

declare function k<T extends { b: any; c?: any }>(
  config: T,
  test: keyof T extends 'b' ? true : false,
): void;


k({
  b: function () {
    return 123;
  },
}, true);

k({
  b: function () {
    return 123;
  },
}, false);
//~^ ERROR: Argument of type 'boolean' is not assignable to parameter of type 'true'.


declare function g<T extends { b: any; c?: any }>(
  test: keyof T extends 'b' ? true : false,
  config: T,
): void;


g(
  true, 
  {
    b: function () {
      return 123;
    },
  }
);

g(
  false, 
//~^ ERROR: Argument of type 'boolean' is not assignable to parameter of type 'true'.
  {
    b: function () {
      return 123;
    },
  }
);