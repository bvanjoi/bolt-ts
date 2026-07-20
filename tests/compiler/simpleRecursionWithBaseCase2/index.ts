// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/simpleRecursionWithBaseCase2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noImplicitAny
//@compiler-options: lib=[esnext]
//@compiler-options: noEmit

async function rec1() {
  if (Math.random() < 0.5) {
    return rec1();
  } else {
    return "hello";
  }
}

let a: Promise<number> = rec1();
//~^ ERROR: Type 'Promise<string>' is not assignable to type 'Promise<number>'.

async function rec2() {
  if (Math.random() < 0.5) {
    return await rec2();
  } else {
    return "hello";
  }
}

async function rec3() {
  return rec3();
}

async function rec4() {
  return await rec4();
}

async function rec5() {
  if (Math.random() < 0.5) {
    return ((rec1()));
  } else {
    return "hello";
  }
}

async function rec6() {
  if (Math.random() < 0.5) {
    return await ((rec1()));
  } else {
    return "hello";
  }
}

declare const ps: Promise<string> | number;

async function foo1() {
  if (Math.random() > 0.5) {
    return ps;
  } else {
    return await foo1();
  }
}

async function foo2() {
  if (Math.random() > 0.5) {
    return ps;
  } else {
    return foo2();
  }
}
