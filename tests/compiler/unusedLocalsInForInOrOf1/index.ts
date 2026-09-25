// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedLocalsInForInOrOf1.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: target=esnext
//@compiler-options: noEmit
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

for (let x of [1, 2]) {
  function f() {
    //~^ ERROR: 'f' is declared but its value is never read.
    x;
  }
}

for (let x of [1, 2]) {
  let f = () => {
    //~^ ERROR: 'f' is declared but its value is never read.
    x;
  };
}

for (const x of [1, 2]) {
  function g() {
    //~^ ERROR: 'g' is declared but its value is never read.
    x;
  }
}

for (let x in { a: 1, b: 2 }) {
  function f2() {
    //~^ ERROR: 'f2' is declared but its value is never read.
    x;
  }
}

for (let x in { a: 1, b: 2 }) {
  let f2 = () => {
    //~^ ERROR: 'f2' is declared but its value is never read.
    x;
  };
}

for (const x in { a: 1, b: 2 }) {
  function g2() {
    //~^ ERROR: 'g2' is declared but its value is never read.
    x;
  }
}

for (let { x } of [{ x: 1 }, { x: 2 }]) {
  function f3() {
    //~^ ERROR: 'f3' is declared but its value is never read.
    x;
  }
}

for (let { x } of [{ x: 1 }, { x: 2 }]) {
  let f3 = () => {
    //~^ ERROR: 'f3' is declared but its value is never read.
    x;
  };
}

for (const { x } of [{ x: 1 }, { x: 2 }]) {
  function g3() {
    //~^ ERROR: 'g3' is declared but its value is never read.
    x;
  }
}
