// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowByClauseExpressionInSwitchTrue1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type A = { type: "A" };
type B = { type: "B" };
type AorB = A | B;

const isA = (x: AorB): x is A => x.type === "A";
const isB = (x: AorB): x is B => x.type === "B";

function test1(x: AorB) {
  switch (true) {
    case isA(x):
      x;
      break;
    case isB(x):
      x;
      break;
  }
}

function test2(x: AorB) {
  switch (true) {
    case isA(x):
      x;
    // fallthrough
    case isB(x):
      x;
      break;
  }
}

let x: string | undefined;

switch (true) {
  case typeof x !== "undefined":
    x.trim();
}

type SomeType = { type: "SomeType" };
declare function isSomeType(x: unknown): x is SomeType;

function processInput(input: string | RegExp | SomeType) {
  switch (true) {
    case typeof input === "string":
      input;
      break;
    case input instanceof RegExp:
      input;
      break;
    case isSomeType(input):
      input;
      break;
  }
}
