// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/anyAndUnknownHaveFalsyComponents.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
//@run-fail

declare let x1: any;
const y1 = x1 && 3;

// #39113
declare let isTreeHeader1: any;
function foo1() {
  return {
    display: "block",
    ...(isTreeHeader1 && {
      display: "flex",
    })
  };
}

declare let x2: unknown;
const y2 = x2 && 3;

// #39113
declare let isTreeHeader2: unknown;
function foo2() {
  return {
    display: "block",
    ...(isTreeHeader1 && {
      display: "flex",
    })
  };
}
