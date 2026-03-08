// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/controlFlowForFunctionLike1.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: noEmit

function test1(a: number | string) {
  if (typeof a === "number") {
    const fn = (arg: typeof a) => true;
    return fn;
  }
  return;
}

test1(0)?.(100);
test1(0)?.("");
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.

function test2(a: number | string) {
  if (typeof a === "number") {
    const fn: { (arg: typeof a): boolean; } = () => true;
    return fn;
  }
  return;
}

test2(0)?.(100);
test2(0)?.("");

function test3(a: number | string) {
  if (typeof a === "number") {
    return (arg: typeof a) => {};
  }
  throw new Error("");
}

test3(1)(100);
test3(1)("");
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.

function test4(a: number | string) {
  let fn = (arg: typeof a) => {};
  if (Math.random() && typeof a === "number") {
    return fn;
  }
  throw new Error("");
}

test4(1)?.(100);
test4(1)?.("");
